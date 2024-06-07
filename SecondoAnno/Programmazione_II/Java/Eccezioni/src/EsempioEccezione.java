public class EsempioEccezione 
{
	public static boolean isNumeric(String s)
	{
		boolean ok = true;
		for(int i=0; i<s.length(); i++)
		{
			ok = ok && (Character.isDigit(s.charAt(i)));
		}
		return ok;
	}
	
	public static void main(String args[])
	{
		int a = 0;
		String s = args[0];
		if (isNumeric(s))
		{
			a = Integer.parseInt(s);
		}
	}
}
