final PVector SCREEN_SIZE = new PVector(640, 480);
final String WINDOW_TITLE = "Triangle to Circle";
final int FRAMERATE = 60;
final float SHAPE_RADIUS = 100;
final int STEPS_FROM_TARGETS = 100;

ArrayList<PVector> shapeVectors;
ArrayList<PVector> targetVectors;
boolean optimization = false;
boolean debugInfo = true;
int stepsToTarget;
int computationTime;


void settings()
{
  size(
    int(SCREEN_SIZE.x),
    int(SCREEN_SIZE.y));
}

void setup()
{
  surface.setTitle(WINDOW_TITLE);
  frameRate(FRAMERATE);
  reset();
}

void draw()
{
  translate(width / 2, height /2);
  long startTime = System.nanoTime();
  
  // Add new vertex and generate new target if target reached
  if (stepsToTarget == 0)
  {
    stepsToTarget = STEPS_FROM_TARGETS;
    shapeVectors.add(shapeVectors.get(shapeVectors.size() - 1).copy());
    
    // OPTIMISATION: Add a shape vector at last vector, then move the second last vector
    // to the midpoint of the third last vector and the last vector.
    if (optimization)
    {
      PVector midpoint = new PVector(
        (shapeVectors.get(shapeVectors.size() - 3).x + shapeVectors.get(shapeVectors.size() - 1).x) / 2,
        (shapeVectors.get(shapeVectors.size() - 3).y + shapeVectors.get(shapeVectors.size() - 1).y) / 2);
      shapeVectors.set(shapeVectors.size() - 2, midpoint);
    }
    
    // Add new target vector, setup the new target shape
    targetVectors.add(new PVector(0, 0));
    for (int index = 0; index < targetVectors.size(); index++)
    {
      float angle = 2 * PI * (float(index) / targetVectors.size());
      targetVectors.set(index, new PVector(SHAPE_RADIUS * cos(angle), SHAPE_RADIUS * sin(angle)));
    }
  }
  
  // Step shape vertices towards target vectors
  for (int index = 0; index < shapeVectors.size(); index++)
  {
    PVector displacement = new PVector(
      shapeVectors.get(index).x - targetVectors.get(index).x,
      shapeVectors.get(index).y - targetVectors.get(index).y);
      
      float step = float(stepsToTarget-1) / float(stepsToTarget);
      shapeVectors.get(index).x = (displacement.x * step) + targetVectors.get(index).x;
      shapeVectors.get(index).y = (displacement.y * step) + targetVectors.get(index).y;
  }
  stepsToTarget -= 1;
  
  // Draw the shape
  background(200);
  fill(255);
  beginShape();
  for (int index = 0; index < shapeVectors.size(); index++)
    vertex(shapeVectors.get(index).x, shapeVectors.get(index).y);
  endShape(CLOSE);
  
  long computationTime = System.nanoTime() - startTime;

  // Draw debug info
  if (debugInfo)
  {
    text("Number of vertices: " + shapeVectors.size(), -width/2, -height/2 + 10);
    text("Optimization: " + optimization, -width/2, -height/2 + 20);
    text("Computation time: " + float(int(computationTime)) / 1000000f + "ms", -width/2, -height/2 + 30);
    for (int index = 0; index < shapeVectors.size(); index++)
    {
      fill(255, 0, 0); circle(shapeVectors.get(index).x, shapeVectors.get(index).y, 5);
      fill(0, 0, 255); circle(targetVectors.get(index).x, targetVectors.get(index).y, 5);
    }
  }
}

void reset()
{
  shapeVectors = new ArrayList<PVector>();
  targetVectors = new ArrayList<PVector>();
  stepsToTarget = 0;
  for (int index = 0; index < 3; index++)
  {
    float angle = 2 * PI * (float(index) / 3);
    shapeVectors.add(new PVector(SHAPE_RADIUS * cos(angle), SHAPE_RADIUS * sin(angle)));
    targetVectors.add(shapeVectors.get(index).copy());
  }
}

void keyPressed()
{
  if (key == 'd')
    debugInfo = !debugInfo;
  
  if (key == 'r')
    reset();
    
  if (key == 'o')
  {
    optimization = !optimization;
    reset();
  }
}
