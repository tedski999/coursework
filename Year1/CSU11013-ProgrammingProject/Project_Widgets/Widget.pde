
// This is the base class for all UI widgets used
public abstract class Widget
{
	protected PVector position;

	// This object must have a subclass with implemented abstract methods
	protected Widget(PVector position)
	{
		this.position = position;
	}

	// These methods can be overriden by the subclass
	protected int wasHoveredOver() { return Actions.NONE; }
	protected int wasNotHoveredOver() { return Actions.NONE; }
	protected int wasClicked() { return Actions.NONE; }
	protected int wasDragged(int mX, int mY) { return Actions.NONE; }

	// These methods must be implemeted by the subclass
	protected abstract void draw();
	protected abstract boolean isMouseOver(int mX, int mY);
}
