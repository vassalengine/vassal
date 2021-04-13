package VASSAL.build;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Test;

public class AbstractBuildableTest {
  public static class FakeBuildable extends AbstractBuildable {
    // Dummy implementations of abstract methods
    public String[] getAttributeNames() { return null; }

    public void setAttribute(String key, Object value) {}

    public String getAttributeValueString(String key) { return null; }

    public void addTo(Buildable parent) {}
  }

  public static interface Parent {}

  public static interface None {}

  public static interface One {}

  public static interface Two {}

  public static interface Three {}

  public static class FakeParent extends FakeBuildable implements Parent {}

  public static class FakeOne extends FakeBuildable implements One {}

  public static class FakeTwo extends FakeBuildable implements Two {}

  public static class FakeThree extends FakeBuildable implements Three {}

  public static class FakeOneThree extends FakeBuildable implements One, Three {}

  private final AbstractBuildable parent;
  private final Buildable c1;
  private final Buildable c2;
  private final Buildable c3;
  private final Buildable c13;

  private final Buildable c1_1;
  private final Buildable c1_1_1;
  private final Buildable c1_2;

  public AbstractBuildableTest() {
    parent = new FakeParent();
    c1 = new FakeOne();
    c2 = new FakeTwo();
    c3 = new FakeThree();
    c13 = new FakeOneThree();

    c1_1 = new FakeOne();
    c1_1_1 = new FakeOne();
    c1_2 = new FakeTwo();

    parent.add(c1);
    parent.add(c2);
    parent.add(c3);
    parent.add(c13);

    c1.add(c1_1);
    c1.add(c1_2);

    c1_1.add(c1_1_1);
  }

  @Test
  public void testGetAllDescendantComponentsFakeBuildable() {
    assertThat(
      parent.getAllDescendantComponentsOf(FakeBuildable.class),
      containsInAnyOrder(parent, c1, c2, c3, c13, c1_1, c1_1_1, c1_2)
    );
  }

  @Test
  public void testGetAllDescendantComponentsFakeParent() {
    assertThat(
      parent.getAllDescendantComponentsOf(FakeParent.class),
      is(List.of(parent))
    );
  }

  @Test
  public void testGetAllDescendantComponentsFakeOne() {
    assertThat(
      parent.getAllDescendantComponentsOf(FakeOne.class),
      is(List.of(c1, c1_1, c1_1_1))
    );
  }

  @Test
  public void testGetAllDescendantComponentsFakeTwo() {
    assertThat(
      parent.getAllDescendantComponentsOf(FakeTwo.class),
      is(List.of(c1_2, c2))
    );
  }

  @Test
  public void testGetAllDescendantComponentsFakeThree() {
    assertThat(
      parent.getAllDescendantComponentsOf(FakeThree.class),
      is(List.of(c3))
    );
  }

  @Test
  public void testGetAllDescendantComponentsNone() {
    assertThat(
      parent.getAllDescendantComponentsOf(None.class),
      is(List.of())
    );
  }

  @Test
  public void testGetAllDescendantComponentsOne() {
    assertThat(
      parent.getAllDescendantComponentsOf(One.class),
      is(List.of(c1, c1_1, c1_1_1, c13))
    );
  }
}
