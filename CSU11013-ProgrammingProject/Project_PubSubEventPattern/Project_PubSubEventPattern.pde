
Button testButton;
SubscriberType1 subscriber1;
SubscriberType2 subscriber2;

void setup()
{
  testButton = new Button();
  subscriber1 = new SubscriberType1();
  subscriber2 = new SubscriberType2();
  
  testButton.addEventListener(subscriber1);
  testButton.addEventListener(subscriber2);
}

void draw()
{
  testButton.update();
}

//===================
// Subscriber

interface OnEventListener
{
    void onEvent();
}

class SubscriberType1 implements OnEventListener
{
  void onEvent()
  {
    println("event captured in sub1!");
  }
}

class SubscriberType2 implements OnEventListener
{
  void onEvent()
  {
    println("event captured in sub2!");
  }
}


//===================
// Publisher

class Widget
{
  ArrayList<OnEventListener> listeners;
  
  Widget()
  {
    listeners = new ArrayList<OnEventListener>();
  }
  
  public void addEventListener(OnEventListener listener)
  {
    listeners.add(listener);
  }
  
  protected void publishEvent()
  {
    for (OnEventListener listener : listeners)
      listener.onEvent();
  }
}

class Button extends  Widget
{
  boolean mouseWasReleased = true;
  
  void update()
  {
    if (mouseWasReleased && mousePressed)
    {
      mouseWasReleased = false;
      publishEvent();
    }
    else if (!mousePressed)
    {
      mouseWasReleased = true;
    }
  }
}
