import bsh.BeanShellExpressionValidator;

public class test_improvements {
    public static void main(String[] args) {
        System.out.println("Testing BeanShellExpressionValidator improvements...");

        // Test 1: Valid expression
        BeanShellExpressionValidator validator1 = new BeanShellExpressionValidator("{6 * 7}");
        System.out.println("Test 1 - Valid expression: " + validator1.isValid());
        System.out.println("Error (should be empty): '" + validator1.getError() + "'");

        // Test 2: Invalid expression
        BeanShellExpressionValidator validator2 = new BeanShellExpressionValidator("{6 * }");
        System.out.println("\nTest 2 - Invalid expression: " + validator2.isValid());
        System.out.println("Error: " + validator2.getError());

        // Test 3: Complex expression with variables and methods
        BeanShellExpressionValidator validator3 = new BeanShellExpressionValidator("{x + y + z.contains('p') + Random(6)}");
        System.out.println("\nTest 3 - Complex expression: " + validator3.isValid());
        System.out.println("Variables: " + validator3.getVariables());
        System.out.println("String variables: " + validator3.getStringVariables());
        System.out.println("Methods: " + validator3.getMethods());
        System.out.println("All variables: " + validator3.getAllVariables());

        // Test 4: Null safety (should throw IllegalArgumentException)
        try {
            BeanShellExpressionValidator validator4 = new BeanShellExpressionValidator(null);
            System.out.println("\nTest 4 - Null safety: FAILED - should have thrown exception");
        } catch (IllegalArgumentException e) {
            System.out.println("\nTest 4 - Null safety: PASSED - " + e.getMessage());
        }

        // Test 5: Empty expression
        BeanShellExpressionValidator validator5 = new BeanShellExpressionValidator("");
        System.out.println("\nTest 5 - Empty expression: " + validator5.isValid());
        System.out.println("Error: " + validator5.getError());

        System.out.println("\nAll tests completed successfully!");
    }
}
