package clojurewerkz.statistiker.libsvm.data;

public class SvmParameter implements Cloneable, java.io.Serializable {
  public int svm_type;
  public int kernel_type;
  public int degree;  // for poly
  public double gamma;  // for poly/rbf/sigmoid
  public double coef0;  // for poly/sigmoid

  // these are for training only
  public double cache_size; // in MB
  public double eps;  // stopping criteria
  public double C;  // for C_SVC, EPSILON_SVR and NU_SVR
  public int nr_weight;    // for C_SVC
  public int[] weight_label;  // for C_SVC
  public double[] weight;    // for C_SVC
  public double nu;  // for NU_SVC, ONE_CLASS, and NU_SVR
  public double p;  // for EPSILON_SVR
  public int shrinking;  // use the shrinking heuristics
  public int probability; // do probability estimates

  public Object clone() {
    try {
      return super.clone();
    } catch (CloneNotSupportedException e) {
      return null;
    }
  }

}
