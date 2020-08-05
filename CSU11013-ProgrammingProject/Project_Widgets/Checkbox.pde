
// A selectedable box
public class Checkbox extends Widget
{
	private boolean isChecked;
	private PVector size;
	private color bgColor;
	private color checkColor;
	private boolean isHighlighted;

	public Checkbox(
		PVector position, PVector size,
		color bgColor, color checkColor)
	{
		super(position);
		this.size = size;
		this.bgColor = bgColor;
		this.checkColor = checkColor;
		isHighlighted = false;
	}

	public void draw()
	{
		strokeWeight(3);
		stroke(isHighlighted ? color(150) : color(0));
		fill(bgColor);
		rect(position.x, position.y, size.x, size.y);

		if (isChecked)
		{
			noStroke();
			fill(checkColor);
			rect(position.x, position.y, size.x - 10, size.y - 10);
		}
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
		isChecked = !isChecked;
		return Actions.NONE;
	}
}
