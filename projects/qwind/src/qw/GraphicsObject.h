#ifndef QW_GRAPHICS_OBJECT_H
#define QW_GRAPHICS_OBJECT_H

#include <qw/config.h>
#include <QGraphicsObject>

namespace qw {

    /**
     * Base for all qw graphics objects. 
     * 
     * Not abstract, so can be used as a container for other graphics items.
     */
    class GraphicsObject: public QGraphicsObject {
        Q_OBJECT;
        Q_PROPERTY(QTransform transform READ transform WRITE setTransform /* TODO: NOTIFY _ */ DESIGNABLE false);
    public:
        GraphicsObject(QGraphicsItem *parent = NULL): QGraphicsObject(parent) {}

        virtual void paint(QPainter *, const QStyleOptionGraphicsItem *, QWidget *) OVERRIDE {
            return;
        }

        virtual QRectF boundingRect() const OVERRIDE {
            return QRectF();
        }
    };

} // namespace qw

#endif // QW_GRAPHICS_OBJECT_H