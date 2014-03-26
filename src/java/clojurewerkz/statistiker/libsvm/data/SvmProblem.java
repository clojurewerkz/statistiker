package clojurewerkz.statistiker.libsvm.data;

public class SvmProblem implements java.io.Serializable {
  public int length;
  public double[] labels;
  public SvmNode[][] datapoints;
}