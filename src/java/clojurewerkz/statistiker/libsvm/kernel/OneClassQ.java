package clojurewerkz.statistiker.libsvm.kernel;

import clojurewerkz.statistiker.libsvm.data.SvmParameter;
import clojurewerkz.statistiker.libsvm.data.SvmProblem;

public class OneClassQ extends Kernel {
  private final KernelCache kernelCache;
  private final double[] QD;

  public OneClassQ(SvmProblem prob, SvmParameter param) {
    super(prob.length, prob.datapoints, param);
    kernelCache = new KernelCache(prob.length, (long) (param.cache_size * (1 << 20)));
    QD = new double[prob.length];
    for (int i = 0; i < prob.length; i++)
      QD[i] = kernel_function(i, i);
  }

  @Override
  public float[] get_Q(int i, int len) {
    float[][] data = new float[1][];
    int start, j;
    if ((start = kernelCache.get_data(i, data, len)) < len) {
      for (j = start; j < len; j++)
        data[0][j] = (float) kernel_function(i, j);
    }
    return data[0];
  }

  @Override
  public double[] get_QD() {
    return QD;
  }

  @Override
  public void swap_index(int i, int j) {
    kernelCache.swap_index(i, j);
    super.swap_index(i, j);
    do {
      double _ = QD[i];
      QD[i] = QD[j];
      QD[j] = _;
    } while (false);
  }
}
