
public class EsempioShape {

	public static void main(String[] args) 
	{
		Shape[] shapes = new Shape[3];
		shapes[0] = new Circle(2.5);
		shapes[1] = new Rectangle(1.2, 3.0);
		shapes[2] = new Rectangle(5.5, 3.8);
		double totalArea = 0;
		
		for (int i=0; i<shapes.length; i++)
		{
			totalArea = totalArea + shapes[i].area();
		}
		
		System.out.println(totalArea);
	}

}
