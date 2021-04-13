package VASSAL.configure;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

import VASSAL.i18n.Resources;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class TranslatingStringEnumConfigurerTest {

  @Test
  public void Test() {

    final String key = "key"; // NON-NLS
    final String name = "name"; // NON-NLS

    final String option1 = "Option 1"; // NON-NLS
    final String option2 = "Option 2"; // NON-NLS
    final String option3 = "Option 3"; // NON-NLS

    final String key1 = "A"; // NON-NLS
    final String key2 = "B"; // NON-NLS
    final String key3 = "C"; // NON-NLS

    final String tran1 = "X"; // NON-NLS
    final String tran2 = "Y"; // NON-NLS
    final String tran3 = "Z"; // NON-NLS

    try (MockedStatic<Resources> staticResources = Mockito.mockStatic(Resources.class)) {

      // Mock a translation of A/B/C to X/Y/Z
      staticResources.when(() -> {
        Resources.getString(key1);
      }).thenReturn(tran1);
      staticResources.when(() -> {
        Resources.getString(key2);
      }).thenReturn(tran2);
      staticResources.when(() -> {
        Resources.getString(key3);
      }).thenReturn(tran3);
    }

    // Check basic functionality is working. All Constructors eventually track through this Constructor.
    TranslatingStringEnumConfigurer config = new TranslatingStringEnumConfigurer(key, name,
      new String[] {option1, option2, option3},
      new String[] {key1, key2, key3});
    config.getControls();

    // Should default to first entry when not specified
    String result = config.getValueString();
    assertThat(result, is(equalTo(option1)));

    // And should change when requested
    config.setValue(option2);
    assertThat(config.getValueString(), is(equalTo(option2)));

    // And not change when a non-valid value is requested, like one of the i18n keys
    config.setValue(key2);
    assertThat(config.getValueString(), is(equalTo(option2)));

    // Value checking/retrieving
    assertThat(config.isValidValue(option3), is(true));
    assertThat(config.isValidValue(key3), is(false));
    assertThat(config.isValidValue(tran3), is(false));

    assertThat(config.getValueIndex(option1), is(0));
    assertThat(config.getValueIndex(option3), is(2));

    // Check Full constructor with Lists
    List<String> listValues = new ArrayList<>();
    listValues.add(option1);
    listValues.add(option2);
    listValues.add(option3);
    List<String> listKeys = new ArrayList<>();
    listKeys.add(key1);
    listKeys.add(key2);
    listKeys.add(key3);

    config = new TranslatingStringEnumConfigurer(key, name, listValues, listKeys, option2);
    assertThat(config.getValueString(), is(equalTo(option2)));

    String[] validValues = config.getValidValues();
    assertThat(validValues[0], is(equalTo(option1)));
    assertThat(validValues[1], is(equalTo(option2)));
    assertThat(validValues[2], is(equalTo(option3)));
  }

}