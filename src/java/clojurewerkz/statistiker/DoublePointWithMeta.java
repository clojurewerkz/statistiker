package clojurewerkz.statistiker;

import org.apache.commons.math3.ml.clustering.DoublePoint;

import java.util.HashMap;
import java.util.Map;

public class DoublePointWithMeta extends DoublePoint {

  private final Map metadata;

  public DoublePointWithMeta(Map metadata, double[] point) {
    super(point);
    this.metadata = metadata;
  }

  public DoublePointWithMeta(Map metadata, int[] point) {
    super(point);
    this.metadata = metadata;
  }

  public Map getMetadata() {
    return this.metadata;
  }
}
