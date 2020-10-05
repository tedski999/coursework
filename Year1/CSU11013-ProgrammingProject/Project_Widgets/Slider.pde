
public class Slider extends Widget
{
	private int currentValue;
	private int midValue;
	private boolean isHighlighted;

	private PVector sliderSize;
	private int spacing;
	private int minValue;
	private int maxValue;
	private color sliderColor;
	private color barColor;
	private PVector textOffset;
	private String textPrepend;
	private int textSize;

	public Slider (
		PVector position, PVector sliderSize, int spacing,
		int minValue, int maxValue, color sliderColor, color barColor,
		PVector textOffset, String textPrepend, int textSize)
	{
		super(position);
		this.sliderSize = sliderSize;
		this.spacing = spacing;
		this.minValue = minValue;
		this.maxValue = maxValue;
		this.sliderColor = sliderColor;
		this.barColor = barColor;
		this.textOffset = textOffset;
		this.textPrepend = textPrepend;
		this.textSize = textSize;
		currentValue = minValue;
		midValue = (maxValue - minValue) / 2 + minValue;
		isHighlighted = false;
	}

	public void draw()
	{
		currentValue = constrain(currentValue, minValue, maxValue);

		noStroke();
		fill(barColor);
		rect(position.x + midValue * spacing, position.y, (maxValue - minValue) * spacing, 2);

		noStroke();
		fill(0);
		textFont(mainFont, textSize);
		text(textPrepend + currentValue, position.x + textOffset.x, position.y + textOffset.y);

		strokeWeight(3);
		stroke(isHighlighted ? color(150) : color(0));
		fill(sliderColor);
		rect(position.x + (currentValue * spacing), position.y, sliderSize.x, sliderSize.y);

	}

	public boolean isMouseOver(int mX, int mY)
	{
		PVector sliderPosition = new PVector(position.x + (currentValue * spacing), position.y);
		return (
			mX > sliderPosition.x - (sliderSize.x / 2) && mX < sliderPosition.x + (sliderSize.x / 2) &&
			mY > sliderPosition.y - (sliderSize.y / 2) && mY < sliderPosition.y + (sliderSize.y / 2)
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
	public int wasDragged(int mX, int mY)
	{
		int valueSelected = int(mX - position.x);
		valueSelected /= spacing;
		currentValue = valueSelected;
		return Actions.NONE;
	}
}
