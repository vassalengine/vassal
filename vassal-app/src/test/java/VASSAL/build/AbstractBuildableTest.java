package VASSAL.build;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import VASSAL.tools.ErrorDialog;

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

  /**
   * A component with one numeric attribute and one which is not, standing in
   * for the Zoomer, DiceButton and Region of #14560 and its family.
   */
  public static class NumericBuildable extends FakeBuildable {
    public int count = -1;
    public String label = "";

    @Override
    public String[] getAttributeNames() {
      return new String[] {"count", "label"};
    }

    @Override
    public void setAttribute(String key, Object value) {
      if ("count".equals(key)) {
        count = Integer.parseInt((String) value);
      }
      else if ("label".equals(key)) {
        label = (String) value;
      }
    }
  }

  private static Element element(String xml) throws Exception {
    return DocumentBuilderFactory.newInstance()
                                 .newDocumentBuilder()
                                 .parse(new InputSource(new StringReader(xml)))
                                 .getDocumentElement();
  }

  /**
   * A value in the buildFile which the component cannot parse as a number is
   * bad data in the module. It used to escape build() as a NumberFormatException,
   * be relabelled "failed to load class" by Builder.create(), and raise an
   * automated bug report against VASSAL while the component vanished.
   * See vassalengine/vassal#14560.
   */
  @Test
  public void aBadNumericAttributeIsReportedAsBadDataBug14560() throws Exception {
    final NumericBuildable b = new NumericBuildable();
    final List<BadDataReport> reports = new ArrayList<>();

    try (MockedStatic<ErrorDialog> ed = mockStatic(ErrorDialog.class)) {
      ed.when(() -> ErrorDialog.dataWarning(any(BadDataReport.class)))
        .thenAnswer(inv -> reports.add(inv.getArgument(0)));

      b.build(element("<x count=\"false\" label=\"kept\"/>"));
    }

    assertThat(reports.size(), is(1));

    // The report names the component, the attribute and the value --- the
    // three things the bug report it replaces carried none of.
    final String data = reports.get(0).getData();
    assertThat(data, containsString(NumericBuildable.class.getName()));
    assertThat(data, containsString("count"));
    assertThat(data, containsString("false"));

    // and the cause is kept, so the error log still shows where it came from
    assertThat(reports.get(0).getCause(), instanceOf(NumberFormatException.class));
  }

  /**
   * The rest of the component is still built: one unreadable attribute costs
   * that attribute, not the whole Zoomer, Dice Button or Region.
   */
  @Test
  public void aBadNumericAttributeDoesNotCostTheWholeComponentBug14560() throws Exception {
    final NumericBuildable b = new NumericBuildable();

    try (MockedStatic<ErrorDialog> ignored = mockStatic(ErrorDialog.class)) {
      b.build(element("<x count=\"false\" label=\"kept\"/>"));
    }

    assertThat(b.label, is("kept"));
    assertThat(b.count, is(-1));
  }

  /** A well-formed value is still applied, and reports nothing. */
  @Test
  public void aGoodNumericAttributeIsUnaffected() throws Exception {
    final NumericBuildable b = new NumericBuildable();
    final List<BadDataReport> reports = new ArrayList<>();

    try (MockedStatic<ErrorDialog> ed = mockStatic(ErrorDialog.class)) {
      ed.when(() -> ErrorDialog.dataWarning(any(BadDataReport.class)))
        .thenAnswer(inv -> reports.add(inv.getArgument(0)));

      b.build(element("<x count=\"7\" label=\"kept\"/>"));
    }

    assertThat(b.count, is(7));
    assertThat(b.label, is("kept"));
    assertThat(reports, is(List.of()));
  }
}
