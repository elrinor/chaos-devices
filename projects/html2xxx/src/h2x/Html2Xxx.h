#ifndef __H2X_HTML2XXX_H__
#define __H2X_HTML2XXX_H__

#include "config.h"
#include <QObject>
#include <QWebPage>
#include <QWebFrame>
#include <QPainter>

namespace h2x {
// -------------------------------------------------------------------------- //
// Html2Xxx
// -------------------------------------------------------------------------- //
  class Html2Xxx: public QObject {
    Q_OBJECT;
  public:
    Html2Xxx(const QUrl &url, const QString& outputFileName, int width): mWidth(width), mOutputFileName(outputFileName) {
      mPage.setPreferredContentsSize(QSize(mWidth, 1));
      mPage.mainFrame()->load(url);
      connect(mPage.networkAccessManager(), SIGNAL(sslErrors(QNetworkReply*, const QList<QSslError>&)), this, SLOT(sslErrors(QNetworkReply*, const QList<QSslError>&)));
      connect(&mPage, SIGNAL(loadFinished(bool)), this, SLOT(render()));
    }

  signals:
    void finished();

  private slots:
    void render() {
      mPage.setViewportSize(mPage.mainFrame()->contentsSize());
      QImage image(mPage.viewportSize(), QImage::Format_ARGB32);
      QPainter painter(&image);
      painter.setRenderHints(QPainter::Antialiasing | QPainter::TextAntialiasing | QPainter::HighQualityAntialiasing | QPainter::SmoothPixmapTransform);
      mPage.mainFrame()->render(&painter);
      painter.end();

      image.save(mOutputFileName);

      emit finished();
    }

    void sslErrors(QNetworkReply* reply, const QList<QSslError>& errors) {
      (void) reply;
      (void) errors;

      return;
    }

  private:
    int mWidth;
    QString mOutputFileName;
    QWebPage mPage;
  };

} // namespace h2x

#endif // __H2X_HTML2XXX_H__
