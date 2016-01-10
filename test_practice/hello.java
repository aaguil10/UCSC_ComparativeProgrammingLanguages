

public class hello { 

   public static void main(String[] args) { 
   	int[] a = {1 ,2 ,3 ,4, 2};
      System.out.println("Hello, World: " + max(a));
   }


   public static int max(int[] list){
   		int largest = list[0];
   		for(int i = 1; i < list.length; i++){
   			if(largest < list[i]){
   				largest = list[i];
   			}
   		}
   		return largest;
   }

}