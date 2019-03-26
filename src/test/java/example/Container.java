package example;

import java.util.List;

public class Container {
   private List<List<Integer>> intListList;

   public Container(List<List<Integer>> intListList) {
      this.intListList = intListList;
   }

   public List<List<Integer>> getIntListList() {
      return intListList;
   }
}
