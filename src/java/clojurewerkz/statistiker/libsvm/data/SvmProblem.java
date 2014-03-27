package clojurewerkz.statistiker.libsvm.data;

public class SvmProblem implements java.io.Serializable {
  public final int length;
  public final double[] labels;
  // TODO: get rid of SvmNode and indexes
  public final SvmNode[][] datapoints;

  public SvmProblem(double[] labels, SvmNode[][] datapoints) {
    assert(labels.length == datapoints.length);
    this.datapoints = datapoints;
    this.labels = labels;
    this.length = labels.length;
  }
}