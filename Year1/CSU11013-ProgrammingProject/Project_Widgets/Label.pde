
// This is a class to provide UI text
public class Label extends Widget
{
	private color textColor;
	private int size;
	private String text;

	public Label(PVector position, color textColor, int size, String text)
	{
		super(position);
		this.textColor = textColor;
		this.size = size;
		this.text = text;
	}

	public void draw()
	{
		noStroke();
		fill(textColor);
		textFont(mainFont, size);
		text(text, position.x, position.y);
	}

	public boolean isMouseOver(int mX, int mY)
	{
		return false;
	}
}
