#ifndef __GAMESTATENOTIFICATIONS_H__
#define __GAMESTATENOTIFICATIONS_H__

#include <QSet>
#include <cassert>
#include "../Game.h"

class GameStateNotificationConsumer {
public:
  virtual void newGame(Game* game) {};

  virtual void startGame() {};

  virtual void makeTurn() {};

  virtual void gameOver() {};

  virtual void closeGame() {};
};


class GameStateNotifier: public GameStateNotificationConsumer {
private:
  QSet<GameStateNotificationConsumer*> notificationConsumers;

public:

#define I_HATE(FUNCNAME, PARAMTYPE, PARAMNAME)                                  \
  virtual void FUNCNAME(PARAMTYPE PARAMNAME) {                                  \
    FOREACH(GameStateNotificationConsumer* consumer, notificationConsumers)     \
      consumer->FUNCNAME(PARAMNAME);                                            \
  };                                                                            \

  I_HATE(newGame, Game*, game)
  I_HATE(startGame, ARX_EMPTY, ARX_EMPTY)
  I_HATE(makeTurn, ARX_EMPTY, ARX_EMPTY)
  I_HATE(gameOver, ARX_EMPTY, ARX_EMPTY)
  I_HATE(closeGame, ARX_EMPTY, ARX_EMPTY)
#undef I_HATE

  void subscribe(GameStateNotificationConsumer* consumer) {
    assert(this->notificationConsumers.find(consumer) == this->notificationConsumers.end());
    this->notificationConsumers.insert(consumer);
  }

  void unsubscribe(GameStateNotificationConsumer* consumer) {
    QSet<GameStateNotificationConsumer*>::iterator pos = this->notificationConsumers.find(consumer);
    assert(pos != this->notificationConsumers.end());
    this->notificationConsumers.erase(pos);
  }
};

#endif