package VASSAL.script;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.script.expression.ExpressionException;

import java.security.SecureRandom;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class ExpressionInterpreterTest {

  @Test
  public void strip() {
    assertThat(ExpressionInterpreter.strip("abc"), is(equalTo("abc")));
    assertThat(ExpressionInterpreter.strip("{abc"), is(equalTo("{abc")));
    assertThat(ExpressionInterpreter.strip("{abc}"), is(equalTo("abc")));
    assertThat(ExpressionInterpreter.strip("{abc}d"), is(equalTo("{abc}d")));
  }

  @Test
  public void getExpression() throws ExpressionException {
    final ExpressionInterpreter interpreter = new ExpressionInterpreter("6 * 7");
    final String s = interpreter.getExpression();
    assertThat(s, is(equalTo("6 * 7")));
  }

  @Test
  public void createInterpreter1() throws ExpressionException {
    // Just test the interpreter is working at all. Exercise custom Vassal functions below.
    ExpressionInterpreter interpreter = new ExpressionInterpreter("6 * 7");
    final String s = interpreter.evaluate();
    assertThat(s, is(equalTo("42")));
  }

  @Test
  public void createInterpreter2() throws ExpressionException {
    // A broken expression should throw an ExpressionException
    assertThrows(ExpressionException.class, () -> new ExpressionInterpreter("6 * "));
  }

  @Test
  public void createInterpreter3() throws ExpressionException {
    // Evaluating a non-existent custom function should throw an ExpressionException
    ExpressionInterpreter interpreter = new ExpressionInterpreter("Blurgh()");
    assertThrows(ExpressionException.class, () -> interpreter.evaluate());

  }

  // Check the Vassal bsh Parser extensions are included:
  // 1. Adding null to an integer does not throw error - 2 + "" = 2
  // 2. Adding a String to an integer converts both to Strings and does not throw error = 2 + "a" = "2a"
  // 3. Allow comparison operators  on strings - "1" == "1" = true, "1" < "2" = true
  // 4. =~ Regexp operator
  // 5. !~ Regexp operator
  @Test
  public void checkExtensions() throws ExpressionException {
    ExpressionInterpreter interpreter = new ExpressionInterpreter("2 + \"\"");
    String s = interpreter.evaluate();
    assertThat(s, is(equalTo("2")));

    interpreter = new ExpressionInterpreter("2 + \"a\"");
    s = interpreter.evaluate();
    assertThat(s, is(equalTo("2a")));

    interpreter = new ExpressionInterpreter("\"1\" == \"1\"");
    s = interpreter.evaluate();
    assertThat(s, is(equalTo("true")));

    interpreter = new ExpressionInterpreter("\"1\" > \"2\"");
    s = interpreter.evaluate();
    assertThat(s, is(equalTo("false")));

    interpreter = new ExpressionInterpreter("\"x\" =~ \"a|x|z\"");
    s = interpreter.evaluate();
    assertThat(s, is(equalTo("true")));

    interpreter = new ExpressionInterpreter("\"x\" !~ \"a|x|z\"");
    s = interpreter.evaluate();
    assertThat(s, is(equalTo("false")));
  }

  @Test
  public void wrap() throws ExpressionException {
    ExpressionInterpreter interpreter = new ExpressionInterpreter("");
    Object o;

    // Null string should remain unchanged
    o = interpreter.wrap("");
    assertThat(o, is(equalTo("")));

    o = interpreter.wrap("true");
    assertThat(o, is(equalTo(Boolean.TRUE)));

    o = interpreter.wrap("false");
    assertThat(o, is(equalTo(Boolean.FALSE)));

    o = interpreter.wrap("42");
    assertThat(o, is(instanceOf(Integer.class)));
    assertThat(o, is(equalTo(42)));

    // Check no interpretation of numeric literals
    o = interpreter.wrap("42L");
    assertThat(o, is(instanceOf(String.class)));
    assertThat(o, is(equalTo("42L")));

    o = interpreter.wrap("42D");
    assertThat(o, is(instanceOf(String.class)));
    assertThat(o, is(equalTo("42D")));

    // And no wrapping of Floats
    o = interpreter.wrap("42.0");
    assertThat(o, is(instanceOf(String.class)));
    assertThat(o, is(equalTo("42.0")));

    o = interpreter.wrap("abc");
    assertThat(o, is(instanceOf(String.class)));
    assertThat(o, is(equalTo("abc")));

  }

  private static final String GETKEY1 = "key1";
  private static final String GETVAL1 = "prop1";
  private static final String GETLVAL1 = "lprop1";
  private static final String GETKEY2 = "key2";
  private static final String GETVAL2 = "1D";
  private static final String GETKEY3 = "key3";
  private static final String GETVAL3 = "1F";
  private static final String GETKEY4 = "key4";
  private static final String GETVAL4 = "1L";

  @Test
  public void getProperty() throws ExpressionException {
    PropertySource ps = new PropertySource() {
      @Override
      public Object getProperty(Object key) {
        if (String.valueOf(key).equals(GETKEY1)) {
          return GETVAL1;
        }
        else if (String.valueOf(key).equals(GETKEY2)) {
          return GETVAL2;
        }
        else if (String.valueOf(key).equals(GETKEY3)) {
          return GETVAL3;
        }
        else if (String.valueOf(key).equals(GETKEY4)) {
          return GETVAL4;
        }
        else {
          return null;
        }
      }
      @Override
      public Object getLocalizedProperty(Object key) {
        if (String.valueOf(key).equals(GETKEY1)) {
          return GETLVAL1;
        }
        else {
          return null;
        }
      }
    };

    // Check GetProperty bsh function
    ExpressionInterpreter interpreter = new ExpressionInterpreter("GetProperty(\"" + GETKEY1 + "\")");
    String result = interpreter.evaluate(ps);
    assertThat(result, is(equalTo(GETVAL1)));

    // Check GetLocalizedProperty bsh function
    interpreter = new ExpressionInterpreter("GetLocalizedProperty(\"" + GETKEY1 + "\")");
    result = interpreter.evaluate(ps);
    assertThat(result, is(equalTo(GETLVAL1)));

    // Regression Test - Check numeric literals returned as properties do not get converted to floating point values
    interpreter = new ExpressionInterpreter(GETKEY2);
    result = interpreter.evaluate(ps);
    assertThat(result, is(equalTo(GETVAL2)));

    interpreter = new ExpressionInterpreter(GETKEY3);
    result = interpreter.evaluate(ps);
    assertThat(result, is(equalTo(GETVAL3)));

    interpreter = new ExpressionInterpreter(GETKEY4);
    result = interpreter.evaluate(ps);
    assertThat(result, is(equalTo(GETVAL4)));
  }

  private static final String MAP_NAME = "myMap";
  private static final String ZONE_NAME = "myZone";
  private static final String ZONE_PROP = "zoneProp";
  private static final String ZONE_VAL = "zoneVal";

  @Test
  public void getZoneProperty() throws ExpressionException {
    Zone zone = mock(Zone.class);
    when(zone.getProperty(ZONE_PROP)).thenReturn(ZONE_VAL);

    Map map = mock(Map.class);
    when(map.getMapName()).thenReturn(MAP_NAME);
    when(map.findZone(ZONE_NAME)).thenReturn(zone);

    GamePiece piece = mock(GamePiece.class);
    when(piece.getMap()).thenReturn(map);

    ExpressionInterpreter interpreter = new ExpressionInterpreter("GetZoneProperty(\"" + ZONE_PROP + "\", \"" + ZONE_NAME + "\")");
    String result = interpreter.evaluate(piece);
    assertThat(result, is(equalTo(ZONE_VAL)));

    try (MockedStatic<Map> staticMap = Mockito.mockStatic(Map.class)) {
      staticMap.when(Map::getMapList).thenReturn(List.of(map));

      interpreter = new ExpressionInterpreter("GetZoneProperty(\"" + ZONE_PROP + "\", \"" + ZONE_NAME + "\", \"" + MAP_NAME + "\")");
      result = interpreter.evaluate(piece);
      assertThat(result, is(equalTo(ZONE_VAL)));

    }
  }

  private static final String MAP_PROP = "mapProp";
  private static final String MAP_VAL = "mapVal";
  @Test
  public void getMapProperty() throws ExpressionException {
    try (MockedStatic<Map> staticMap = Mockito.mockStatic(Map.class)) {
      Map map = mock(Map.class);
      when(map.getMapName()).thenReturn(MAP_NAME);
      when(map.getProperty(MAP_PROP)).thenReturn(MAP_VAL);
      staticMap.when(Map::getMapList).thenReturn(List.of(map));

      ExpressionInterpreter interpreter = new ExpressionInterpreter("GetMapProperty(\"" + MAP_PROP + "\", \"" + MAP_NAME + "\")");
      String result = interpreter.evaluate();
      assertThat(result, is(equalTo(MAP_VAL)));

    }
  }

  private static final String SUM_PROP = "count";

  @Test
  public void sumStack() throws ExpressionException {
    Stack s = new Stack();

    BasicPiece bp1 = new BasicPiece();
    bp1.setProperty(SUM_PROP, "24");

    BasicPiece bp2 = new BasicPiece();
    bp2.setProperty(SUM_PROP, "18");

    s.add(bp1);
    s.add(bp2);

    ExpressionInterpreter interpreter = new ExpressionInterpreter("SumStack(\"" + SUM_PROP + "\")");
    String result = interpreter.evaluate(bp1);
    assertThat(result, is(equalTo("42")));

  }

  @Test
  public void random() throws ExpressionException {

    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      GameModule gm = mock(GameModule.class);
      when(gm.getRNG()).thenReturn(new SecureRandom());

      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      ExpressionInterpreter interpreter = new ExpressionInterpreter("Random(4)");
      String result = interpreter.evaluate();
      assertThat(result, is(greaterThanOrEqualTo("1")));
      assertThat(result, is(lessThanOrEqualTo("4")));

      interpreter = new ExpressionInterpreter("Random(-1, 1)");
      result = interpreter.evaluate();
      assertThat(result, is(greaterThanOrEqualTo("-1")));
      assertThat(result, is(lessThanOrEqualTo("1")));
    }
  }

  @Test
  public void isRandom() throws ExpressionException {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      GameModule gm = mock(GameModule.class);
      when(gm.getRNG()).thenReturn(new SecureRandom());

      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      ExpressionInterpreter interpreter = new ExpressionInterpreter("IsRandom()");
      String result = interpreter.evaluate();

      interpreter = new ExpressionInterpreter("IsRandom(0)");
      result = interpreter.evaluate();
      assertThat(result, is(equalTo("false")));

      interpreter = new ExpressionInterpreter("IsRandom(100)");
      result = interpreter.evaluate();
      assertThat(result, is(equalTo("true")));
    }
  }

  private static final String MATCH_PROP = "match";
  private static final String MATCH_VAL = "pickMe";
  private static final String MATCH_EXPR = "{match==\\\"pickMe\\\"}";
  private static final String MAP_NAME_2 = "myMap";

  @Test
  public void sum_and_count() throws ExpressionException {
    try (MockedStatic<Map> staticMap = Mockito.mockStatic(Map.class)) {
      BasicPiece bp1 = new BasicPiece();
      bp1.setProperty(SUM_PROP, "2");
      bp1.setProperty(MATCH_PROP, MATCH_VAL);
      bp1.setProperty(Properties.PIECE_ID, "1");

      BasicPiece bp2 = new BasicPiece();
      bp2.setProperty(SUM_PROP, "4");
      bp2.setProperty(MATCH_PROP, "dontPickMe");
      bp2.setProperty(Properties.PIECE_ID, "2");

      BasicPiece bp3 = new BasicPiece();
      bp3.setProperty(SUM_PROP, "8");
      bp3.setProperty(MATCH_PROP, MATCH_VAL);
      bp3.setProperty(Properties.PIECE_ID, "3");

      BasicPiece bp4 = new BasicPiece();
      bp4.setProperty(SUM_PROP, "16");
      bp4.setProperty(MATCH_PROP, MATCH_VAL);
      bp4.setProperty(Properties.PIECE_ID, "4");

      Map map1 = mock(Map.class);
      when(map1.getMapName()).thenReturn(MAP_NAME);
      when(map1.getAllPieces()).thenReturn(new GamePiece[] {bp4});
      Map map2 = mock(Map.class);
      when(map2.getMapName()).thenReturn(MAP_NAME_2);
      when(map2.getAllPieces()).thenReturn(new GamePiece[] {bp1, bp2, bp3});
      bp1.setMap(map2);
      bp2.setMap(map2);
      bp3.setMap(map2);
      bp4.setMap(map1);
      staticMap.when(Map::getMapList).thenReturn(List.of(map1, map2));

      // Sum Test 1 - No map name, should select pieces 1, 3 & 4 across both maps.
      ExpressionInterpreter interpreter = new ExpressionInterpreter("Sum(\"" + SUM_PROP + "\", \"" + MATCH_EXPR + "\")");
      String result = interpreter.evaluate(bp1);
      assertThat("Sum properties across multiple maps", result, is(equalTo("26")));

      // Sum Test 2 - Map name provided,  should select pieces 1 and 3
      interpreter = new ExpressionInterpreter("Sum(\"" + SUM_PROP + "\", \"" + MATCH_EXPR + "\", \"" + MAP_NAME_2 + "\")");
      result = interpreter.evaluate(bp1);
      assertThat("Sum properties on named map", result, is(equalTo("10")));

      // Count Test 1 - No map name, should select pieces 1, 3 & 4 across both maps.
      interpreter = new ExpressionInterpreter("Count(\"" + MATCH_EXPR + "\")");
      result = interpreter.evaluate(bp1);
      assertThat("Count across multiple maps", result, is(equalTo("3")));

      // Count Test 2 - Map name provided,  should select pieces 1 and 3
      interpreter = new ExpressionInterpreter("Count(\"" + MATCH_EXPR + "\", \"" + MAP_NAME_2 + "\")");
      result = interpreter.evaluate(bp1);
      assertThat("Count on named map", result, is(equalTo("2")));
    }
  }

}
