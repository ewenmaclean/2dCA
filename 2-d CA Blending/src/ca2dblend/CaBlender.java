package ca2dblend;

import ca2dblend.GridView2;
import ca2dblend.Field;

public class CaBlender {

	
	public static void main( String[] args )
	{

		GridView2 view = new GridView2(80, 120);
		Field field = new Field(80, 120);
		view.showStatus(0, field);
		
	}		

}
