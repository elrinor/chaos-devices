#ifndef QW_MAIN_WINDOW_H
#define QW_MAIN_WINDOW_H

#include <qw/config.h>
#include <QMainWindow>
#include "InterpolatedAnimation.h"

class QGraphicsScene;
class QAbstractAnimation;

namespace Ui {
    class MainWindow;
}

namespace qw {
    class GraphicsObject;

    /**
     * Main application window.
     */
    class MainWindow: public QMainWindow {
        Q_OBJECT;
    public:
        MainWindow(QWidget *parent = NULL);

        virtual ~MainWindow();

    protected:
        enum State {
            INVALID,            /**< Invalid state. */
            INITIAL,            /**< Initial placement. */
            STACKING,           /**< Animating forward stack arrangement. */
            UNSTACKING,         /**< Animating backward stack arrangement. */
            STACK,              /**< In stack. */
            ROLLING_FORWARD,    /**< Rolling forward. */
            ROLLING_BACKWARD    /**< Rolling backward. */
        };

        /**
         * Performs transition from the current state to the given state.
         * 
         * \param state                New state.
         */
        void transition(State state);

        /**
         * Event filter for graphics view.
         */
        virtual bool eventFilter(QObject *watched, QEvent *event) OVERRIDE;

    protected slots:
        void at_animation_finished();

    private:
        /**
         * Starts transformation-based animation.
         */
        void startAnimation(const QList<QTransform> &start, const QList<QTransform> &end, int msecs, int startFrom);

        /**
         * Starts stack rolling animation.
         */
        void startAnimation(bool forward, int msecs);

        /**
         * \returns                    List of current object transformations.
         */
        QList<QTransform> currentTransform() const;

        /** This window's ui. */
        QScopedPointer<Ui::MainWindow> mUi;

        /** Graphics scene. */
        QGraphicsScene *mScene;

        /** List of graphics objects on the scene. */
        QList<GraphicsObject *> mObjects;

        /** Current scene state. */
        State mState;

        /** Current animation. */
        QAbstractAnimation *mAnimation;

        /** List of initial objects' transformations. */
        QList<QTransform> mInitialTransforms;

        /** List of stacked objects' transformations. */
        QList<QTransform> mStackTransforms;

        /** Interpolator to use for interpolation when rolling stack. */
        Interpolator mInterpolator;

        /* Flags for currently pressed keys. */
        bool mLeftPressed, mRightPressed, mCtrlPressed;
    };

} // namespace qw

#endif // QW_MAIN_WINDOW_H
