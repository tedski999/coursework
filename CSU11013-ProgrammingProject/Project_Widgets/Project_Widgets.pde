
// Window constants
final String   WINDOW_TITLE = "Widgets";
final PVector  WINDOW_SIZE = new PVector(800, 600);
final int      FRAMERATE = 60;

// Global objects
ArrayList<Screen> screenList;
int currentScreenIndex;
color currentRectColor;
boolean isExiting;
PFont mainFont;

// Used to setup the processing window
void settings()
{
	size(int(WINDOW_SIZE.x), int(WINDOW_SIZE.y));
}

// Used to initialize app
void setup()
{
	surface.setTitle(WINDOW_TITLE);
	surface.setIcon(loadImage("icon.png"));
	frameRate(FRAMERATE);
	rectMode(CENTER);
	textAlign(CENTER, CENTER);
	ellipseMode(RADIUS);

	mainFont = loadFont("LucidaConsole-48.vlw");
	initScreens();

	currentRectColor = color(255, 255, 255);
	isExiting = false;
}

// Adds all the widgets to the scene
void initScreens()
{
	Screen screen1, screen2;
	screen1 = new Screen(color(50));
	screen2 = new Screen(color(200));


	//=========================
	// Screen 1 widgets

	screen1.addWidget(new Label(new PVector(200, 20), color(100), 30, "Welcome to Screen 1"));

	// Main buttons
	screen1.addWidget(
		new Button(
			new PVector(600, 180), Actions.SET_SCREEN_INDEX_TO_ONE,
			new PVector(100, 25), color(220),
			"Forward", 15));
	screen1.addWidget(
		new Button(
			new PVector(600, 100), Actions.PRINT_HELLO_FROM_1,
			new PVector(200, 100), color(255, 255, 0),
			"Button 1", 15));

	// Colored square controls
	screen1.addWidget(
		new Button(
			new PVector(50, 450), Actions.SET_RECT_COLOR_RED,
			new PVector(40, 40), color(255, 0, 0),
			"red", 10));
	screen1.addWidget(
		new Button(
			new PVector(100, 450), Actions.SET_RECT_COLOR_GREEN,
			new PVector(40, 40), color(0, 255, 0),
			"green", 10));
	screen1.addWidget(
		new Button(
			new PVector(150, 450), Actions.SET_RECT_COLOR_BLUE,
			new PVector(40, 40), color(0, 0, 255),
			"blue", 10));
	screen1.addWidget(new Label(new PVector(100, 490), color(255), 10, "Click a button to\nchange it's color"));

	// Radio buttons example
	screen1.addWidget(
		new RadioButtonSet(
			new PVector(525, 250), new PVector(0, 50), 15,
			4, color(220), color(0, 0, 255)));
	screen1.addWidget(new Label(new PVector(600, 250), color(255), 12, "Option 1"));
	screen1.addWidget(new Label(new PVector(600, 300), color(255), 12, "Choice 2"));
	screen1.addWidget(new Label(new PVector(600, 350), color(255), 12, "Perference 3"));
	screen1.addWidget(new Label(new PVector(600, 400), color(255), 12, "Wrong Opinion"));

	// Exit button
	screen1.addWidget(
		new Button(
			new PVector(700, 525), Actions.EXIT_APP,
			new PVector(75, 50), color(255, 0, 0),
			"Exit", 15));


	//=========================
	// Screen 2 widgets

	screen2.addWidget(new Label(new PVector(100, 20), color(200, 0, 0), 20, "Wow! Look! It's Screen 2"));
	screen2.addWidget(new Label(new PVector(100, 490), color(0), 10, "Controls on Screen 1..."));

	// Main buttons
	screen2.addWidget(
		new Button(
			new PVector(600, 180), Actions.SET_SCREEN_INDEX_TO_ZERO,
			new PVector(100, 25), color(220, 220, 220),
			"Backward", 15));
	screen2.addWidget(
		new Button(
			new PVector(600, 100), Actions.PRINT_HELLO_FROM_2,
			new PVector(200, 100), color(0, 255, 255),
			"Button 2", 15));

	// Checkboxes
	screen2.addWidget(new Label(new PVector(600, 280), color(0), 10, "Check out these checkboxes"));
	screen2.addWidget(
		new Checkbox(
			new PVector(575, 310), new PVector(30, 30),
			color(220), color(0, 0, 255)));
	screen2.addWidget(
		new Checkbox(
			new PVector(625, 310), new PVector(30, 30),
			color(220), color(0, 0, 255)));

	// Sliders example
	screen2.addWidget(
		new Slider(
			new PVector(50, 100), new PVector(15, 30), 3,
			0, 100, color(255, 0, 0), color(50),
			new PVector(100, 25), "This sliders value: ", 12));
	screen2.addWidget(
		new Slider(
			new PVector(50, 150), new PVector(15, 30), 3,
			0, 100, color(0, 255, 0), color(50),
			new PVector(100, 25), "Green value -> ", 12));
	screen2.addWidget(
		new Slider(
			new PVector(50, 200), new PVector(15, 30), 3,
			0, 100, color(0, 0, 255), color(50),
			new PVector(325, 0), "", 12));

	// Exit button
	screen2.addWidget(
		new Button(
			new PVector(700, 525), Actions.EXIT_APP,
			new PVector(75, 50), color(255, 0, 0),
			"Exit", 15));


	screenList = new ArrayList<Screen>();
	screenList.add(screen1);
	screenList.add(screen2);
}

// Called every 1/FRAMERATE seconds
void draw()
{
	// Exiting app
	if (isExiting)
		exit();

	// Draw current screen
	screenList.get(currentScreenIndex).draw();

	// Draw test square
	strokeWeight(5);
	stroke(0);
	fill(currentRectColor);
	rect(100, 535, 50, 50);
}

// Called every time the mouse cursor is moved
void mouseMoved()
{
	Screen currentScreen = screenList.get(currentScreenIndex);
	int actionId = currentScreen.mouseMovedEvent(mouseX, mouseY);
	doAction(actionId);
}

// Called every time the left mouse button is pressed down
void mousePressed()
{
	Screen currentScreen = screenList.get(currentScreenIndex);
	int actionId = currentScreen.mousePressedEvent(mouseX, mouseY);
	doAction(actionId);
}

// Called every time the left mouse button is released
void mouseReleased()
{
	Screen currentScreen = screenList.get(currentScreenIndex);
	int actionId = currentScreen.mouseReleasedEvent(mouseX, mouseY);
	doAction(actionId);
}

// Called every time mouse moves while held down
void mouseDragged()
{
	Screen currentScreen = screenList.get(currentScreenIndex);
	int actionId = currentScreen.mouseDraggedEvent(mouseX, mouseY);
	doAction(actionId);
}

// Performs any action denoted by the provided action id
void doAction(int actionId)
{
	switch (actionId)
	{
		case Actions.NONE:
			break;

		case Actions.EXIT_APP:
			isExiting = true;
			break;

		case Actions.SET_SCREEN_INDEX_TO_ZERO:
			currentScreenIndex = 0;
			break;

		case Actions.SET_SCREEN_INDEX_TO_ONE:
			currentScreenIndex = 1;
			break;

		case Actions.PRINT_HELLO_FROM_1:
			println("Hello from Screen 1!");
			break;

		case Actions.PRINT_HELLO_FROM_2:
			println("Howdy from Screen 2!");
			break;

		case Actions.SET_RECT_COLOR_RED:
			currentRectColor = color(255, 0, 0);
			break;

		case Actions.SET_RECT_COLOR_GREEN:
			currentRectColor = color(0, 255, 0);
			break;

		case Actions.SET_RECT_COLOR_BLUE:
			currentRectColor = color(0, 0, 255);
			break;

		default:
			println("Warning: No action with id " + actionId + " found!");
			break;
	}
}
