package VASSAL.configure;

public class PropertyExpressionConfigurer extends StringConfigurer {

  public PropertyExpressionConfigurer(String key, String name) {
    super(key, name);
  }

  public PropertyExpressionConfigurer(String key, String name, PropertyExpression expression) {
    super(key, name, expression.getExpression());
  }
}