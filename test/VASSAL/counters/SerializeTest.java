package VASSAL.counters;

import java.lang.reflect.Constructor;

import VASSAL.build.MockModuleTest;



public abstract class SerializeTest<T extends Decorator> extends MockModuleTest {

	public void serializeTest(Class<T>clazz, T trait) throws Exception {
		BasicPiece basicPiece = new BasicPiece();
		trait.setInner(basicPiece);
		String typeString = trait.myGetType();

		Constructor<T> constructor = clazz.getConstructor(String.class, GamePiece.class);
		T deserialized = constructor.newInstance(typeString, null);
		assertSame(trait, deserialized);
	}

	abstract void assertSame(T t1, T t2);
}
