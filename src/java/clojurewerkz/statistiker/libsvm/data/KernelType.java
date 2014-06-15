package clojurewerkz.statistiker.libsvm.data;

import java.io.IOException;
import java.nio.file.Files;

public class KernelType {
  public static final int LINEAR = 0;
  public static final int POLY = 1;
  public static final int RBF = 2;
  public static final int SIGMOID = 3;
  public static final int PRECOMPUTED = 4;

  public void main() {
    try {
      Files.createTempDirectory("");
    } catch (IOException e) {
      e.printStackTrace();
    }

  }
}
