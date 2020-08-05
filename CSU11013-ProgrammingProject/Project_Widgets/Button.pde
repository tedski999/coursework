
// A rectangle with a border and center label
public class Button extends Widget
{
	private int actionId;
	private PVector size;
	private color bgColor;
	private String text;
	private int textSize;
	private boolean isHighlighted;

	public Button(
		PVector position, int actionId,
		PVector size, color bgColor,
		String text, int textSize)
	{
		super(position);
		this.actionId = actionId;
		this.size = size;
		this.bgColor = bgColor;
		this.text = text;
		this.textSize = textSize;
		isHighlighted = false;
	}

	public void draw()
	{
		strokeWeight(3);
		stroke(isHighlighted ? color(255) : color(0));
		fill(bgColor);
		rect(position.x, position.y, size.x, size.y);

		noStroke();
		fill(0);
		textFont(mainFont, textSize);
		text(text, position.x, position.y);
	}

	public boolean isMouseOver(int mX, int mY)
	{
		return (
			mX > position.x - (size.x / 2) && mX < position.x + (size.x / 2) &&
			mY > position.y - (size.y / 2) && mY < position.y + (size.y / 2)
		);
	}

	@Override
	public int wasHoveredOver()
	{
		isHighlighted = true;
		return Actions.NONE;
	}

	@Override
	public int wasNotHoveredOver()
	{
		isHighlighted = false;
		return Actions.NONE;
	}

	@Override
	public int wasClicked()
	{
		return actionId;
	}
}
