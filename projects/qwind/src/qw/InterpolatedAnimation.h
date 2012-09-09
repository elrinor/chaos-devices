#ifndef QW_INTERPOLATED_ANIMATION_H
#define QW_INTERPOLATED_ANIMATION_H

#include <qw/config.h>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/function.hpp>
#include <QVariantAnimation>

namespace qw {

    typedef boost::function<QVariant(QVariant, QVariant, qreal)> Interpolator;


    /**
     * Animation class extension that allows to set interpolation function on 
     * a per-animation basis. 
     * 
     * Unfortunately, QPropertyAnimation forcefully converts stored variants
     * to the type of the property it is bound to, so interpolating from one
     * space to another is not possible.
     * 
     * Note that Base must be derived from QVariantAnimation.
     */
    template<class Base>
    class InterpolatedAnimation: public Base {
        static_assert(boost::is_base_of<QVariantAnimation, Base>::value, "Base must be derived from QVariantAnimation.");
    public:
        InterpolatedAnimation(QObject *parent = NULL): Base(parent) {}

        const qw::Interpolator &interpolator() const {
            return mInterpolator;
        }

        void setInterpolator(const qw::Interpolator &interpolator) {
            mInterpolator = interpolator;
        }

    protected:
        virtual QVariant interpolated(const QVariant &from, const QVariant &to, qreal progress) const OVERRIDE {
            if(!mInterpolator.empty()) {
                return mInterpolator(from, to, progress);
            } else {
                return Base::interpolated(from, to, progress);
            }
        }

    private:
        /** Function used for interpolation. */
        // TODO: animations are short-lived, interpolators are not. Think about using a pointer?
        qw::Interpolator mInterpolator;
    };


} // namespace qw

#endif // QW_INTERPOLATED_ANIMATION_H
