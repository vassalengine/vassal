package VASSAL.tools.nio.file.zipfs;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import static org.junit.Assert.assertEquals;

import VASSAL.tools.nio.file.zipfs.ZipUtils; 

@RunWith(Suite.class)
@SuiteClasses({
  ZipUtilsTest.DosToJavaTimeTest.class,
  ZipUtilsTest.JavaToDosTimeTest.class
})
public class ZipUtilsTest {

  @RunWith(Parameterized.class)
  public static class DosToJavaTimeTest {
    protected final long ztime;
    protected final long rtime;

    public DosToJavaTimeTest(long ztime, long rtime) {
      this.ztime = ztime;
      this.rtime = rtime;
    }

    @Test
    public void test() {
      assertEquals(rtime, ZipUtils.dosToJavaTime(ztime));
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        {    2162688L,  315532800000L }, //  1 Jan 1980 00:00:00.000 UTC
        { 1010806140L, 1264974236000L }, // 31 Jan 2010 21:43:56.000 UTC
        { 1949505987L, 2147483646000L }, // 19 Jan 2038 03:14:06.000 UTC
        { 1949505988L, 2147483648000L }, // 19 Jan 2038 03:14:08.000 UTC
        { 4288659325L, 4354819198000L }, // 31 Dec 2107 23:59:58.000 UTC
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class JavaToDosTimeTest {
    protected final long ztime;
    protected final long rtime;

    public JavaToDosTimeTest(long rtime, long ztime) {
      this.rtime = rtime;
      this.ztime = ztime;
    }

    @Test
    public void test() {
      assertEquals(ztime, ZipUtils.javaToDosTime(rtime));
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        {  315532800000L,    2162688L }, //  1 Jan 1980 00:00:00.000 UTC
        { 1264974236000L, 1010806140L }, // 31 Jan 2010 21:43:56.000 UTC
        { 2147483646000L, 1949505987L }, // 19 Jan 2038 03:14:06.000 UTC
        { 2147483647000L, 1949505987L }, // 19 Jan 2038 03:14:07.000 UTC
        { 2147483648000L, 1949505988L }, // 19 Jan 2038 03:14:08.000 UTC
        { 4354819198000L, 4288659325L }, // 31 Dec 2107 23:59:58.000 UTC
      });
    }
  }
}
