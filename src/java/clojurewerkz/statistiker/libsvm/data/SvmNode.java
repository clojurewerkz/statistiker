package clojurewerkz.statistiker.libsvm.data;

public class SvmNode implements java.io.Serializable {
  public final int index;
  public final double value;

  public SvmNode(int index, double value) {
    this.index = index;
    this.value = value;
  }
}
