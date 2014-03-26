package clojurewerkz.statistiker.libsvm.data;

import clojurewerkz.statistiker.libsvm.data.SvmNode;
import clojurewerkz.statistiker.libsvm.data.SvmParameter;

/**
 * SVM Model
 */
public class SvmModel implements java.io.Serializable {
  public SvmParameter param;  // parameter
  public int nr_class;    // number of classes, = 2 in regression/one class SVM
  public int l;      // total #SV
  public SvmNode[][] SV;  // SVs (SV[length])
  public double[][] sv_coef;  // coefficients for SVs in decision functions (sv_coef[k-1][length])
  public double[] rho;    // constants in decision functions (rho[k*(k-1)/2])
  public double[] probA;         // pariwise probability information
  public double[] probB;

  // for classification only

  public int[] label;    // label of each class (label[k])
  public int[] nSV;    // number of SVs for each class (nSV[k])
  // nSV[0] + nSV[1] + ... + nSV[k-1] = length
};
