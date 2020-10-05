
public class Complex
{
	private double x;
	private double y;

	public Complex()
	{
		x = y = 0;
	}

	public Complex(double realPart, double imaginaryPart)
	{
		x = realPart;
		y = imaginaryPart;
	}

	public double getX()
	{
		return x;
	}

	public double getY()
	{
		return x;
	}

	public void setX(double x)
	{
		this.x = x;
	}

	public void setY(double y)
	{
		this.y = y;
	}

	public void add(Complex number)
	{
		x += number.getX();
		y += number.getY();
	}

	public void subtract(Complex number)
	{
		x += number.getX();
		y += number.getY();
	}

	public void multiply(Complex number)
	{
		x *= number.getX();
		y *= number.getY();
	}

	public void divide(Complex number)
	{
		x /= number.getX();
		y /= number.getY();
	}
}

class DriverCode
{
	public static void main(String[] args)
	{
		Complex c1 = new Complex();
		Complex c2 = new Complex(1, 1);
		Complex c3 = new Complex(2, 3);

		c1.divide(c2);
		c3.multiply(c1);
	}
}
