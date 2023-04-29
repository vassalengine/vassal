package VASSAL.tools.qtree;

import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class QuadTreeTest extends junit.framework.TestCase {

    private QuadTree<String> getTree() {
        QuadTree<String> qt = new QuadTree<String>(0, 0, 100, 100);
        qt.set(5, 20, "Foo");
        qt.set(50, 32, "Bar");
        qt.set(47, 96, "Baz");
        qt.set(50, 50, "Bing");
        qt.set(12, 0, "Bong");
        return qt;
    }

    private <T> void assertTreesChildrenAreNull(QuadTree<T> qt) {
        QNode<T> root = qt.getRootNode();
        assertNull("NE should be null", root.getNe());
        assertNull("NW should be null", root.getNw());
        assertNull("SE should be null", root.getSe());
        assertNull("SW should be null", root.getSw());
    }

    @Test
    public void testGetCount() {
        QuadTree<String> qt = getTree();
        assertEquals("Count should be 5", 5, qt.getCount());
        qt.remove(50, 32);
        assertEquals("Count should be 4", 4, qt.getCount());
    }

    @Test
    public void testGetKeys() {
        QPoint<String>[] keys = getTree().getKeys();
        Arrays.sort(keys);
        String keyString = Arrays.asList(keys).toString();
        String expected = "[(5.0, 20.0), (12.0, 0.0), (47.0, 96.0), (50.0, 32.0), (50.0, 50.0)]";
        assertEquals("Sorted keys should be " + expected, expected, keyString);
    }

    @Test
    public void testGetValues() {
        List<String> values = getTree().getValues();
        Collections.sort(values);
        String valueString = values.toString();
        assertEquals("Sorted values should be [Bar, Baz, Bing, Bong, Foo]", "[Bar, Baz, Bing, Bong, Foo]", valueString);
    }

    @Test
    public void testContains() {
        QuadTree<String> qt = getTree();
        assertTrue("Should contain (5, 20)", qt.contains(5, 20));
        assertFalse("Should not contain (13, 13)", qt.contains(13, 13));
    }

    @Test
    public void testSearchIntersects() {
        QuadTree<String> qt = getTree();
        QPoint<String>[] points = qt.searchIntersect(4, 0, 51, 98);
        Arrays.sort(points);
        String keyString = Arrays.asList(points).toString();
        String expected = "[(5.0, 20.0), (12.0, 0.0), (47.0, 96.0), (50.0, 32.0), (50.0, 50.0)]";
        assertEquals("Sorted keys should be " + expected, expected, keyString);


        QPoint<String>[] points2 = qt.searchIntersect(5, 0, 50, 96);
        Arrays.sort(points2);
        String keyString2 = Arrays.asList(points).toString();
        String expected2 = "[(5.0, 20.0), (12.0, 0.0), (47.0, 96.0), (50.0, 32.0), (50.0, 50.0)]";
        assertEquals("Sorted keys should be " + expected2, expected2, keyString2);

        QPoint<String>[] points3 = qt.searchIntersect(55, 0, 50, 96);
        assertEquals("Should return no points for higher x",0,points3.length);
    }

    @Test
    public void testSearchWithin() {
        QuadTree<String> qt = getTree();
        QPoint<String>[] points = qt.searchWithin(4, -1, 51, 98);
        Arrays.sort(points);
        String keyString = Arrays.asList(points).toString();
        String expected = "[(5.0, 20.0), (12.0, 0.0), (47.0, 96.0), (50.0, 32.0), (50.0, 50.0)]";
        assertEquals("Sorted keys should be " + expected, expected, keyString);
    }

    @Test
    public void testClear() {
        QuadTree<String> qt = getTree();
        qt.clear();
        assertTrue("Tree should be empty", qt.isEmpty());
        assertFalse("Tree should not contain (5, 20)", qt.contains(5, 20));
    }

    @Test
    public void testConstructor() {
        QuadTree<?> qt = new QuadTree<Object>(-10, -5, 6, 12);
        QNode<?> root = qt.getRootNode();
        assertEquals("X of root should be -10.0", -10.0, root.getX());
        assertEquals("Y of root should be -5.0", -5.0, root.getY());
        assertEquals("Width of root should be 16.0", 16.0, root.getW());
        assertEquals("Height of root should be 17.0", 17.0, root.getH());
        assertTrue("Tree should be empty", qt.isEmpty());
    }

    @Test
    public void testClone() {
        QuadTree<String> qt = getTree().clone();
        assertFalse("Clone should not be empty", qt.isEmpty());
        assertTrue("Should contain (47, 96)", qt.contains(47, 96));
    }

    @Test
    public void testRemove() {
        QuadTree<String> qt = getTree();
        assertEquals("(5, 20) should be removed", "Foo", qt.remove(5, 20));
        assertEquals("(5, 20) should be removed", "Bar", qt.remove(50, 32));
        assertEquals("(5, 20) should be removed", "Baz", qt.remove(47, 96));
        assertEquals("(5, 20) should be removed", "Bing", qt.remove(50, 50));
        assertEquals("(5, 20) should be removed", "Bong", qt.remove(12, 0));
        assertNull("(6, 6) wasn\"t there to remove", qt.remove(6, 6));
        assertTrue("Tree should be empty", qt.isEmpty());
        assertTreesChildrenAreNull(qt);
    }

    @Test
    public void testIsEmpty() {
        QuadTree<String> qt = getTree();
        qt.clear();
        assertTrue(qt.isEmpty());
        assertEquals("Root should  be empty node", QNodeType.EMPTY, qt.getRootNode().getNodeType());
        assertTreesChildrenAreNull(qt);
    }

    @Test
    public void testBalancing() {
        QuadTree<String> qt = new QuadTree<String>(0, 0, 100, 100);
        QNode<String> root = qt.getRootNode();

        // Add a point to the NW quadrant.
        qt.set(25, 25, "first");

        assertEquals("Root should be a leaf node.",
          QNodeType.LEAF, root.getNodeType());
        assertTreesChildrenAreNull(qt);

        assertEquals("first", root.getPoint().getValue());

        // Add another point in the NW quadrant
        qt.set(25, 30, "second");

        assertEquals("Root should now be a pointer.",
          QNodeType.POINTER, root.getNodeType());
        assertNotNull("NE should be not be null", root.getNe());
        assertNotNull("NW should be not be null", root.getNw());
        assertNotNull("SE should be not be null", root.getSe());
        assertNotNull("SW should be not be null", root.getSw());
        assertNull(root.getPoint());

        // Delete the second point.
        qt.remove(25, 30);

        assertEquals("Root should have been rebalanced and be a leaf node.",
          QNodeType.LEAF, root.getNodeType());
        assertTreesChildrenAreNull(qt);
        assertEquals("first", root.getPoint().getValue());
    }

    @Test
    public void testTreeBounds() {
        QuadTree<String> qt = getTree();
        assertFails(qt, -10, -10, "1");
        assertFails(qt, -10, 10, "2");
        assertFails(qt, 10, -10, "3");
        assertFails(qt, -10, 110, "4");
        assertFails(qt, 10, 130, "5");
        assertFails(qt, 110, -10, "6");
        assertFails(qt, 150, 14, "7");
    }

    public <T> void assertFails(QuadTree<T> qt, double x, double y, T value) {
        try {
            qt.set(x, y, value);
            fail();
        } catch (Exception ex) {
            assertEquals("Out of bounds : (" + x + ", " + y + ")", ex.getMessage());
        }
    }
}