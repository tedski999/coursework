
// A selectedable box
public class RadioButtonSet extends Widget
{
	private int indexChecked;
	private int latestHighlightedButtonIndex;

	private PVector buttonOffsets;
	private int buttonRadius;
	private int buttonCount;
	private color bgColor;
	private color checkColor;

	public RadioButtonSet(
		PVector position, PVector buttonOffsets, int buttonRadius,
		int buttonCount, color bgColor, color checkColor)
	{
		super(position);
		this.buttonOffsets = buttonOffsets;
		this.buttonRadius = buttonRadius;
		this.buttonCount = buttonCount;
		this.bgColor = bgColor;
		this.checkColor = checkColor;
		indexChecked = 0;
		latestHighlightedButtonIndex = -1;
	}

	public void draw()
	{
		strokeWeight(3);
		PVector buttonPosition = position.copy();
		for (int index = 0; index < buttonCount; index++)
		{
			stroke(index == latestHighlightedButtonIndex ? color(150) : color(0));
			fill(bgColor);
			circle(buttonPosition.x, buttonPosition.y, buttonRadius);
			if (index == indexChecked)
			{
				noStroke();
				fill(checkColor);
				circle(buttonPosition.x, buttonPosition.y, buttonRadius - 5);
			}

			buttonPosition.add(buttonOffsets);
		}
	}

	public boolean isMouseOver(int mX, int mY)
	{
		PVector buttonPosition = position.copy();
		for (int index = 0; index < buttonCount; index++)
		{
			float distance = sqrt(sq(buttonPosition.x - mX) + sq(buttonPosition.y - mY));
			if (distance < buttonRadius)
			{
				latestHighlightedButtonIndex = index;
				return true;
			}

			buttonPosition.add(buttonOffsets);
		}

		latestHighlightedButtonIndex = -1;
		return false;
	}

	@Override
	public int wasClicked()
	{
		indexChecked = latestHighlightedButtonIndex;
		return Actions.NONE;
	}
}
