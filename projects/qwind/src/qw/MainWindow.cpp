#include "MainWindow.h"
#include <cassert>
#include <Eigen/Dense>
#include <Eigen/Geometry>
#include <boost/foreach.hpp>
#include <boost/range/algorithm/find_if.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <QDir>
#include <QPixmap>
#include <QResizeEvent>
#include <QKeyEvent>
#include <QPropertyAnimation>
#include <QParallelAnimationGroup>
#include <QSequentialAnimationGroup>
#include "GraphicsObject.h"
#include "ui_MainWindow.h"

namespace qw {

    namespace {
        static const char *extensions[] = {
            ".png",
            ".jpg",
            ".jpeg",
            ".bmp"
        };

        QTransform operator+(const QTransform &l, const QTransform &r) {
            return QTransform(
                l.m11() + r.m11(),
                l.m12() + r.m12(),
                l.m13() + r.m13(),
                l.m21() + r.m21(),
                l.m22() + r.m22(),
                l.m23() + r.m23(),
                l.m31() + r.m31(),
                l.m32() + r.m32(),
                l.m33() + r.m33()
            );
        }

        QVariant transformInterpolator(const QTransform &start, const QTransform &end, qreal progress) {
            return start * (1 - progress) + end * progress;
        }

        /* The following hacks are needed to use our custom interpolator with QPropertyAnimation. */

        QVariant wrapTransform(qreal value) {
            return QTransform(value, 0, 0, 0, 0, 0);
        }

        qreal unwrapTransform(const QVariant &value) {
            return value.value<QTransform>().m11();
        }
    }

    MainWindow::MainWindow(QWidget *parent): 
        QMainWindow(parent),
        mUi(new Ui::MainWindow()),
        mState(INVALID),
        mAnimation(NULL),
        mLeftPressed(false),
        mRightPressed(false),
        mCtrlPressed(false)
    {
        mUi->setupUi(this);
        
        /* Register standard interpolator for QTransforms. */
        qRegisterAnimationInterpolator<QTransform>(transformInterpolator);

        /* Create stacking interpolator. */
        mInterpolator = [&](const QVariant &start, const QVariant &end, qreal givenProgress) -> QVariant {
            using namespace Eigen;

            double progress = unwrapTransform(start) * (1 - givenProgress) + unwrapTransform(end) * givenProgress;

            /* Prepare scaling transform constructor. The one from Eigen gives some very strange effects. */ // TODO: investigate
            auto scale = [](double x, double y, double z, double w) -> Projective3d {
                Eigen::Projective3d result;
                result.matrix().fill(0);
                result.matrix().diagonal() = Eigen::Vector4d(x, y, z, w);
                return result;
            };

            /* That's the transform where we're going to accumulate our transformation. 
             * Note that accumulated transforms will be applied to the input vector in
             * reverse order. */
            Projective3d t;

            /* Fill in frustum projective transform. Use 90 degree FOV. */
            const double left = -1;
            const double right = 1;
            const double bottom = -1;
            const double top = 1;
            const double near = 1;
            const double far = 101;
            t(0, 0) = 2 * near / (right - left);
            t(0, 1) = 0;
            t(0, 2) = (right + left) / (right - left);
            t(0, 3) = 0;
            t(1, 0) = 0;
            t(1, 1) = 2 * near / (top - bottom);
            t(1, 2) = (top + bottom) / (top - bottom);
            t(1, 3) = 0;
            t(2, 0) = 0;
            t(2, 1) = 0;
            t(2, 2) = - (far + near) / (far - near);
            t(2, 3) = - 2 * far * near / (far - near);
            t(3, 0) = 0;
            t(3, 1) = 0;
            t(3, 2) = -1;
            t(3, 3) = 0;

            /* Look at positive Z, make Y point up. */
            t = t * scale(1, -1, -1, 1);

            /* Move in front of the camera. */
            t *= Translation3d(-0.5, 0.5, 3);

            /* Rotate stack round the Y axis. */
            t *= AngleAxisd(-M_PI * 60 / 180, Vector3d(0, 1, 0));

            /* Position stack based on progress. */
            t *= Translation3d(Vector3d(0, 0.5 - 1 * progress * progress, 2 - 4 * progress));

            /* Rotate sprite round the Y axis to compensate for stack rotation. */
            t *= AngleAxisd(M_PI * 45 / 180, Vector3d(0, 1, 0));

            /* Rotate sprite round the X axis based on progress. */
            t *= AngleAxisd(M_PI * 10 / 180 * (1 - progress), Vector3d(1, 0, 0));

            /* Mirror sprites. */
            t = t * scale(1, -1, 1, 1);

            /* Map [-1, 1]x[-1, 1] square and create resulting QTransform. */
            Vector4d src[4] = {
                Vector4d(-1, -1, 0, 1),
                Vector4d(-1,  1, 0, 1),
                Vector4d( 1,  1, 0, 1),
                Vector4d( 1, -1, 0, 1)
            };

            Vector4d dst[4];
            boost::transform(src, boost::begin(dst), [&](const Vector4d &v) -> Vector4d { 
                Eigen::Vector4d tmp = t * v;
                return tmp / tmp[3];
            });

            auto mapper = [](const Vector4d (&src)[4]) -> QPolygonF {
                QPolygonF result;
                foreach(const Eigen::Vector4d &v, src)
                    result.append(QPointF(v[0], v[1]));
                return result;
            };

            QTransform result;
            QTransform::quadToQuad(mapper(src), mapper(dst), result);
            return result;
        };

        /* Prepare scene & view. */
        mScene = new QGraphicsScene(this);
        mScene->setBackgroundBrush(QBrush(QColor(Qt::black)));
        mUi->graphicsView->setScene(mScene);
        mUi->graphicsView->installEventFilter(this);
        mUi->graphicsView->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

        /* Create graphics items for graphics files in current dir. */
        QDir dir;
        dir.setFilter(QDir::Files | QDir::Readable);
        foreach(const QFileInfo &info, dir.entryInfoList()) {
            QString path = info.absoluteFilePath();
            
            if(boost::find_if(extensions, [&](const char *ext) { return path.endsWith(QLatin1String(ext)); }) == boost::end(extensions))
                continue;

            QPixmap pixmap(path);
            if(pixmap.isNull())
                continue;

            GraphicsObject *object = new GraphicsObject();
            QGraphicsPixmapItem *item = new QGraphicsPixmapItem(pixmap, object);
            
            /* Make it fit into [-1, 1] x [-1, 1] square. */
            double scale = 2.0 / qMax(pixmap.width(), pixmap.height());
            QTransform transform;
            transform.translate(-1, -1);
            transform.scale(scale, scale);
            item->setTransform(transform);

            mObjects.push_back(object);
            mScene->addItem(object);
        }

        /* Initialize transformations. */
        qreal objectWidth = 2.0 / mObjects.size();

        for(int i = 0; i < mObjects.size(); i++) {
            QTransform transform;
            transform.translate(-1 + objectWidth * 0.5 + i * objectWidth, 0);
            transform.scale(objectWidth / 2.0, objectWidth / 2.0); 

            mInitialTransforms.push_back(transform);
        }
            
        for(int i = 0; i < mObjects.size(); i++)
            mStackTransforms.append(mInterpolator(wrapTransform(0.0), wrapTransform(1.0), 1.0 * i / mObjects.size()).value<QTransform>());

        /* Init state. */
        transition(INITIAL);
    }

    QList<QTransform> MainWindow::currentTransform() const {
        QList<QTransform> result;
        foreach(qw::GraphicsObject *object, mObjects)
            result.push_back(object->transform());
        return result;
    }

    void MainWindow::startAnimation(bool forward, int msecs) {
        if(mAnimation != NULL) {
            delete mAnimation;
            mAnimation = NULL;
        }

        if(!forward) {
            mObjects.push_back(mObjects.front());
            mObjects.pop_front();
        }

        qreal delta = 1.0 / mObjects.size();

        /* Create animation of objects from the middle of the stack. */
        QParallelAnimationGroup *group = new QParallelAnimationGroup(this);
        for(int i = 0; i < mObjects.size() - 1; i++) {
            {
                /* Animate motion. */
                InterpolatedAnimation<QPropertyAnimation> *animation = new InterpolatedAnimation<QPropertyAnimation>();
                animation->setInterpolator(mInterpolator);
                animation->setTargetObject(mObjects[i]);
                animation->setPropertyName("transform");
                animation->setStartValue(wrapTransform(delta * i));
                animation->setEndValue(wrapTransform(delta * (i + 1)));
                animation->setDuration(msecs);
                group->addAnimation(animation);
            }
            {
                /* Animate z. */
                // TODO: animating Z-values may not be a good idea. Test.
                QPropertyAnimation *animation = new QPropertyAnimation(mObjects[i], "z");
                animation->setStartValue(i);
                animation->setEndValue(i + 1);
                animation->setDuration(msecs);
                group->addAnimation(animation);
            }
        }
        
        /* Create animation for the border object. */
        QObject *object = mObjects[mObjects.size() - 1];

        /* Animate opacity. */
        auto createOpacityAnimation = [](QObject *object, qreal start, qreal end, int msecs) -> QAbstractAnimation * {
            QPropertyAnimation *animation = new QPropertyAnimation(object, "opacity");
            animation->setStartValue(start);
            animation->setEndValue(end);
            animation->setDuration(msecs);
            return animation;
        };

        QSequentialAnimationGroup *opacityGroup = new QSequentialAnimationGroup();
        opacityGroup->addAnimation(createOpacityAnimation(object, 1.0, 0, msecs / 2));
        opacityGroup->addAnimation(createOpacityAnimation(object, 0, 1.0, msecs / 2));
        group->addAnimation(opacityGroup);

        /* Animate motion. */
        auto createMotionAnimation = [&](QObject *object, qreal start, qreal end, int msecs) -> QAbstractAnimation * {
            InterpolatedAnimation<QPropertyAnimation> *animation = new InterpolatedAnimation<QPropertyAnimation>();
            animation->setInterpolator(mInterpolator);
            animation->setTargetObject(object);
            animation->setPropertyName("transform");
            animation->setStartValue(wrapTransform(start));
            animation->setEndValue(wrapTransform(end));
            animation->setDuration(msecs);
            return animation;
        };

        QSequentialAnimationGroup *motionGroup = new QSequentialAnimationGroup();
        motionGroup->addAnimation(createMotionAnimation(object, delta * (mObjects.size() - 1), delta * (mObjects.size() - 0.5), msecs / 2));
        motionGroup->addAnimation(createMotionAnimation(object, delta * -0.5, 0, msecs / 2));
        group->addAnimation(motionGroup);
        
        /* Animate z. */
        auto createZAnimation = [&](QObject *object, qreal start, qreal end, int msecs) -> QAbstractAnimation * {
            QPropertyAnimation *animation = new QPropertyAnimation(object, "z");
            animation->setStartValue(start);
            animation->setEndValue(end);
            animation->setDuration(msecs);
            return animation;
        };

        QSequentialAnimationGroup *zGroup = new QSequentialAnimationGroup();
        zGroup->addAnimation(createZAnimation(object, mObjects.size() - 1, mObjects.size() - 0.5, msecs / 2));
        zGroup->addAnimation(createZAnimation(object, -0.5, 0, msecs / 2));
        group->addAnimation(zGroup);

        /* Start it. */
        connect(group, SIGNAL(finished()), this, SLOT(at_animation_finished()));
        group->setDirection(forward ? QAbstractAnimation::Forward : QAbstractAnimation::Backward);
        group->start();
        mAnimation = group;
    }

    void MainWindow::startAnimation(const QList<QTransform> &start, const QList<QTransform> &end, int msecs, int startFrom) {
        if(mAnimation != NULL) {
            delete mAnimation;
            mAnimation = NULL;
        }

        QParallelAnimationGroup *group = new QParallelAnimationGroup(this);
        for(int i = 0; i < mObjects.size(); i++) {
            QPropertyAnimation *animation = new QPropertyAnimation(mObjects[i], "transform");
            animation->setStartValue(start[i]);
            animation->setEndValue(end[i]);
            animation->setDuration(msecs);
            group->addAnimation(animation);
        }

        connect(group, SIGNAL(finished()), this, SLOT(at_animation_finished()));
        group->setCurrentTime(startFrom);
        group->start();
        mAnimation = group;
    }

    void MainWindow::transition(State state) {
        if(mState == state)
            return;

        if(mObjects.empty())
            return;

        switch(mState) {
        case INVALID:
            switch(state) {
            case INITIAL:
                for(int i = 0; i < mObjects.size(); i++)
                    mObjects[i]->setTransform(mInitialTransforms[i]);
                break;
            default:
                unreachable();
            }
            break;
        case INITIAL: 
            switch(state) {
            case STACKING:
                startAnimation(mInitialTransforms, mStackTransforms, 250, 0);
                break;
            default:
                unreachable();
            }
            break;
        case STACKING:
            switch(state) {
            case UNSTACKING:
                startAnimation(currentTransform(), mInitialTransforms, 250, mAnimation->currentTime());
                break;
            case STACK:
                break;
            default:
                unreachable();
            }
            break;
        case UNSTACKING:
            switch(state) {
            case INITIAL:
                break;
            case STACKING:
                startAnimation(currentTransform(), mStackTransforms, 250, mAnimation->currentTime());
                break;
            default:
                unreachable();
            }
            break;
        case STACK:
            switch(state) {
            case UNSTACKING:
                startAnimation(mStackTransforms, mInitialTransforms, 250, 0);
                break;
            case ROLLING_FORWARD:
                startAnimation(true, 250);
                break;
            case ROLLING_BACKWARD:
                startAnimation(false, 250);
                break;
            default:
                unreachable();
            }
            break;
        case ROLLING_BACKWARD:
        case ROLLING_FORWARD:
            switch(state) {
            case STACK:
                break;
            default:
                unreachable();
            }
            break;
        default:
            unreachable();
        }

        mState = state;
    }

    void MainWindow::at_animation_finished() {
        switch(mState) {
        case STACKING:
            transition(STACK);
            break;
        case UNSTACKING:
            transition(INITIAL);
            break;
        case ROLLING_FORWARD:
            mObjects.push_front(mObjects.back());
            mObjects.pop_back();
            /* Fall through. */
        case ROLLING_BACKWARD:
            transition(STACK);

            if(!mCtrlPressed) {
                transition(UNSTACKING);
            } else if(mLeftPressed || mRightPressed) {
                transition(mLeftPressed ? ROLLING_BACKWARD : ROLLING_FORWARD);
            }
            break;
        default:
            unreachable();
        }
    }

    bool MainWindow::eventFilter(QObject *watched, QEvent *event) {
        switch(event->type()) {
        case QEvent::Resize: {
            QResizeEvent *e = static_cast<QResizeEvent *>(event);

            qreal scaleFactor = e->size().width() / 2.0;

            /* Maps scene's [-1, 1] x segment to view's x. */
            mUi->graphicsView->resetTransform();
            mUi->graphicsView->scale(scaleFactor, scaleFactor);

            return QMainWindow::eventFilter(watched, event);
        }
        case QEvent::KeyPress: {
            QKeyEvent *e = static_cast<QKeyEvent *>(event);

            if(e->key() == Qt::Key_Control)
                mCtrlPressed = true;

            if(e->key() == Qt::Key_Left)
                mLeftPressed = true;

            if(e->key() == Qt::Key_Right)
                mRightPressed = true;

            if((mState == INITIAL || mState == UNSTACKING) && mCtrlPressed)
                transition(STACKING);

            if(mState == STACK && (mLeftPressed || mRightPressed))
                transition(mLeftPressed ? ROLLING_BACKWARD : ROLLING_FORWARD);

            return QMainWindow::eventFilter(watched, event);
        }
        case QEvent::KeyRelease: {
            QKeyEvent *e = static_cast<QKeyEvent *>(event);

            if(e->key() == Qt::Key_Control)
                mCtrlPressed = false;

            if(e->key() == Qt::Key_Left)
                mLeftPressed = false;

            if(e->key() == Qt::Key_Right)
                mRightPressed = false;

            if((mState == STACK || mState == STACKING) && !mCtrlPressed)
                transition(UNSTACKING);

            return QMainWindow::eventFilter(watched, event);
        }
        default:
            return QMainWindow::eventFilter(watched, event);
        }
    }

    MainWindow::~MainWindow() {
        return;
    }

} // namespace qw