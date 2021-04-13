package VASSAL.counters;


import VASSAL.tools.NamedKeyStroke;
import java.lang.reflect.InvocationTargetException;
import javax.swing.KeyStroke;
import org.junit.jupiter.api.Test;

public class DeselectTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    Deselect trait = new Deselect();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = new Deselect();
    trait.commandName = "testCommand"; // NON-NLS
    trait.key = NamedKeyStroke.of("xyzzy"); // NON-NLS
    trait.description = "plugh"; // NON-NLS
    trait.unstack = true;
    serializeTest("NamedKeyStroke", trait); // NON-NLS

    // Set a Command and standard KeyStroke
    trait = new Deselect();
    trait.commandName = "testCommand"; // NON-NLS
    trait.key = NamedKeyStroke.of(KeyStroke.getKeyStroke(65, 0));
    trait.description = "plugh"; // NON-NLS
    trait.unstack = true;
    serializeTest("KeyStroke", trait); // NON-NLS

  }
}