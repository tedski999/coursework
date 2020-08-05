
public class Screen
{
	private ArrayList<Widget> widgetList;
	private color bgColor;
	private Widget draggedWidget;

	public Screen(color bgColor)
	{
		this.widgetList = new ArrayList<Widget>();
		this.bgColor = bgColor;
	}

	// Draws this screen and its widgets to the processing window
	void draw()
	{
		background(bgColor);
		for (Widget widget : widgetList)
			widget.draw();
	}

	// Adds a widget to this screen objects list of widgets
	void addWidget(Widget newWidget)
	{
		widgetList.add(newWidget);
	}

	// Return the id of the action to be done if the mouse was over one of the widgets during a movement event
	int mouseMovedEvent(int mX, int mY)
	{
		int actionId = Actions.NONE;
		for (Widget widget : widgetList)
			if (widget.isMouseOver(mX, mY))
				actionId = widget.wasHoveredOver();
			else
				widget.wasNotHoveredOver();
		return actionId;
	}

	// Return the id of the action to be done if the mouse was over one of the widgets during a click event
	int mousePressedEvent(int mX, int mY)
	{
		int actionId = Actions.NONE;
		for (Widget widget : widgetList)
		{
			if (widget.isMouseOver(mouseX, mouseY))
			{
				draggedWidget = widget;
				actionId = widget.wasClicked();
			}
		}

		return actionId;
	}

	// Releases the widget that was being dragged
	int mouseReleasedEvent(int mX, int mY)
	{
		draggedWidget = null;
		return Actions.NONE;
	}

	// Tells the dragged widget that the mouse was moved
	int mouseDraggedEvent(int mX, int mY)
	{
		if (draggedWidget != null)
			draggedWidget.wasDragged(mX, mY);
		return Actions.NONE;
	}
}
