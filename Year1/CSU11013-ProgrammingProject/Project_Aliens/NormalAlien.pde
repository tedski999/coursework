
public class NormalAlien extends Alien
{
  /* private enum AlienState { forward, backward, downward }; // Enums not supported in Processing 3.5.4 */
  public final static int STATE_FORWARD = 0;
  public final static int STATE_BACKWARD = 1;
  public final static int STATE_DOWNWARD = 2;
  
  private final float STARTING_SPEED = 2f;
  private final float ACCELERATION = 0.25f;
  
  private float currentSpeed;
  private int currentState;
  private int nextState;
  private float targetYPosition;
  
  public NormalAlien(PVector position, PImage aliveTexture, PImage deadTexture)
  {
    this.position = position;
    this.aliveTexture = aliveTexture;
    this.deadTexture = deadTexture;
    currentTexture = this.aliveTexture;
    currentState = STATE_FORWARD;
    currentSpeed = STARTING_SPEED;
  }
  
  @Override
  public void update()
  {
    if (isAlive)
    {      
      switch (currentState)
      {
        case (NormalAlien.STATE_FORWARD):
          moveForward();
          break;
        
        case (NormalAlien.STATE_BACKWARD):
          moveBackward();
          break;
        
        case (NormalAlien.STATE_DOWNWARD):
          moveDownward();
          break;
        
        default:
          break;
      }
    }
  }
  
  private void moveForward()
  {
    position.x += currentSpeed;
    if (position.x >= SCREEN_SIZE.x - currentTexture.width / 2)
    {
      currentState = NormalAlien.STATE_DOWNWARD;
      nextState = NormalAlien.STATE_BACKWARD;
      targetYPosition = position.y + aliveTexture.height;
    }
  }
  
  private void moveBackward()
  {
    position.x -= currentSpeed;
    if (position.x <= currentTexture.width / 2)
    {
      currentState = NormalAlien.STATE_DOWNWARD;
      nextState = NormalAlien.STATE_FORWARD;
      targetYPosition = position.y + aliveTexture.height;
    } 
  }
  
  private void moveDownward()
  {
    position.y += currentSpeed;
    if (position.y >= targetYPosition)
    {
      currentSpeed += ACCELERATION;
      currentState = nextState;
    }
  }
};
