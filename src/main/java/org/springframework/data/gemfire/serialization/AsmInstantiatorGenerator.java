/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.serialization;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.geode.DataSerializable;
import org.apache.geode.Instantiator;
import org.springframework.asm.ClassWriter;
import org.springframework.asm.FieldVisitor;
import org.springframework.asm.MethodVisitor;
import org.springframework.asm.Opcodes;
import org.springframework.asm.Type;
import org.springframework.beans.BeanUtils;
import org.springframework.util.Assert;

/**
 * ASM based {@link InstantiatorGenerator} implementation. This class relies on ASM 2.x package
 * repacked by Spring framework to minimize the number of dependencies and avoid any versioning
 * confusion.
 *
 * @author Costin Leau
 */
public class AsmInstantiatorGenerator implements InstantiatorGenerator, Opcodes {

	private static final String PKG = "org/springextensions/gef/serialization";
	private static final String CLASS_LABEL = "Instantiator$Synthetic";
	private static final String INSTANTIATOR_NAME = Type.getInternalName(Instantiator.class);
	private static final String SERIALIZABLE_NAME = Type.getInternalName(Serializable.class);
	private static final String CLASS_DESCRIPTOR = Type.getDescriptor(Class.class);
	private static final String CLASS_FIELD_NAME = "clazz";
	private static final String ID_FIELD_NAME = "classId";

	private static final String INIT = "<init>";
	private static final String CINIT = "<clinit>";
	private static final String NEW_INSTANCE = "newInstance";
	private static final String NEW_INSTANCE_DESC = Type.getMethodDescriptor(Type.getType(DataSerializable.class),
			new Type[] {});

	// generated class counter
	private static final AtomicLong counter = new AtomicLong(1);

	// class cache
	private final ConcurrentMap<Class<? extends DataSerializable>, Instantiator> cache = new ConcurrentHashMap<Class<? extends DataSerializable>, Instantiator>();


	private static final class BytecodeClassLoader extends ClassLoader {

		public BytecodeClassLoader(ClassLoader loader) {
			super(loader);
		}

		public Class<?> loadClass(String name, byte[] bytecode) {
			return defineClass(name, bytecode, 0, bytecode.length);
		}
	}

	private final BytecodeClassLoader classLoader;

	public AsmInstantiatorGenerator() {
		this(AsmInstantiatorGenerator.class.getClassLoader());
	}

	public AsmInstantiatorGenerator(final ClassLoader classLoader) {
		Assert.notNull(classLoader);
		this.classLoader = AccessController.doPrivileged(new PrivilegedAction<BytecodeClassLoader>() {
			public BytecodeClassLoader run() {
				return new BytecodeClassLoader(classLoader);
			}
		});
	}

	public Instantiator getInstantiator(Class<? extends DataSerializable> clazz, int classId) {
		Instantiator instantiator = cache.get(clazz);
		if (instantiator == null) {
			synchronized (cache) {
				instantiator = cache.get(clazz);
				if (instantiator == null) {
					// create Instantiator
					instantiator = createInstantiator(clazz, classId);
					cache.putIfAbsent(clazz, instantiator);
				}
			}
		}
		return instantiator;
	}

	/**
	 * Returns an instance of the custom instantiator created for the given class.
	 *
	 * @param clazz
	 * @param classId
	 * @return
	 */
	private Instantiator createInstantiator(Class<? extends DataSerializable> clazz, int classId) {
		validateClass(clazz);
		Class<?> clz = createCustomInstantiatorClass(clazz, classId);
		return (Instantiator) BeanUtils.instantiate(clz);
	}

	/**
	 * Does basic sanity checks to make sure the constructor can be properly invoked by our generated
	 * class.
	 *
	 * @param clazz
	 */
	private void validateClass(Class<? extends DataSerializable> clazz) {
		Assert.isTrue(!Modifier.isAbstract(clazz.getModifiers()), "Cannot instantiate abstract classes");
		Assert.isTrue(Modifier.isPublic(clazz.getModifiers()), "Only public classes are supported");
		try {
			Constructor<? extends DataSerializable> ctor = clazz.getConstructor();
			Assert.isTrue(Modifier.isPublic(ctor.getModifiers()), "Default constructor is not public");

		} catch (Exception ex) {
			throw new IllegalArgumentException("Class " + clazz + " unsuitable for instantiation", ex);
		}
	}

	/**
	 * Generates a new Instantiator class for the given custom class.
	 *
	 * The generated class has the following definition:
	 *
	 * <pre>
	 * package org.springframework.data.gemfire.serialization;
	 *
	 * public class &lt;<i>T</i>>Instantiator$Synthetic<i>Counter</i> extends Instantiator implements Serializable {
	 *
	 *  private static final Class&lt;<i>T</i>> clazz = T.class;
	 *  private static final int classId = <i>value</i>;
	 *
	 *  public DateInstantiator() {
	 *     this(clazz, classId);
	 *  }
	 *
	 *  public DateInstantiator(Class<? extends DataSerializable> c, int classId) {
	 *     super(c, classId);
	 *  }
	 *
	 *  public <i>T</i> newInstance() {
	 *     return new <i>T</i>();
	 *  }
	 * }
	 * </pre>
	 *
	 * @param clazz
	 * @return
	 */
	Class<?> createCustomInstantiatorClass(Class<? extends DataSerializable> clazz, int classId) {
		String classInternalName = PKG + clazz.getSimpleName() + CLASS_LABEL + counter.getAndIncrement();
		byte[] bytecode = generateClassBytecode(classInternalName, clazz, classId);
		// translate internal name to binary form
		return classLoader.loadClass(classInternalName.replace('/', '.'), bytecode);
	}

	byte[] generateClassBytecode(String className, Class<? extends DataSerializable> clazz, int classId) {
		ClassWriter cw = new ClassWriter(0);

		cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, className, null, INSTANTIATOR_NAME, new String[] { SERIALIZABLE_NAME });
		FieldVisitor fv = cw.visitField(ACC_PRIVATE + ACC_FINAL + ACC_STATIC, CLASS_FIELD_NAME, CLASS_DESCRIPTOR, null,
				null);
		fv.visitEnd();
		fv = cw.visitField(ACC_PRIVATE + ACC_FINAL + ACC_STATIC, ID_FIELD_NAME, Type.INT_TYPE.getDescriptor(), null,
				Integer.valueOf(classId));
		fv.visitEnd();

		String voidNoArgMethodDescriptor = Type.getMethodDescriptor(Type.VOID_TYPE, new Type[] {});

		// field class loading
		MethodVisitor mv = cw.visitMethod(ACC_STATIC, CINIT, voidNoArgMethodDescriptor, null, null);
		mv.visitCode();
		mv.visitLdcInsn(Type.getType(clazz));
		mv.visitFieldInsn(PUTSTATIC, className, CLASS_FIELD_NAME, CLASS_DESCRIPTOR);
		mv.visitInsn(RETURN);
		mv.visitMaxs(1, 0);
		mv.visitEnd();

		String voidArgClassAndIntDescriptor = Type.getMethodDescriptor(Type.VOID_TYPE, new Type[] {
				Type.getType(Class.class), Type.INT_TYPE });

		// default constructor
		mv = cw.visitMethod(ACC_PUBLIC, INIT, voidNoArgMethodDescriptor, null, null);
		mv.visitCode();
		mv.visitVarInsn(ALOAD, 0);
		mv.visitFieldInsn(GETSTATIC, className, CLASS_FIELD_NAME, CLASS_DESCRIPTOR);
		mv.visitFieldInsn(GETSTATIC, className, ID_FIELD_NAME, Type.INT_TYPE.getDescriptor());
		mv.visitMethodInsn(INVOKESPECIAL, className, INIT, voidArgClassAndIntDescriptor);
		mv.visitInsn(RETURN);
		mv.visitMaxs(3, 1);
		mv.visitEnd();

		// two-arg constructor
		mv = cw.visitMethod(ACC_PUBLIC, INIT, voidArgClassAndIntDescriptor, null, null);
		mv.visitCode();
		mv.visitVarInsn(ALOAD, 0);
		mv.visitVarInsn(ALOAD, 1);
		mv.visitVarInsn(ILOAD, 2);
		mv.visitMethodInsn(INVOKESPECIAL, INSTANTIATOR_NAME, INIT, voidArgClassAndIntDescriptor);
		mv.visitInsn(RETURN);
		mv.visitMaxs(3, 3);
		mv.visitEnd();

		Type customClassType = Type.getType(clazz);
		String customTypeNoArgDesc = Type.getMethodDescriptor(customClassType, new Type[] {});

		// newInstance overloaded method
		mv = cw.visitMethod(ACC_PUBLIC, NEW_INSTANCE, customTypeNoArgDesc, null, null);

		mv.visitCode();
		mv.visitTypeInsn(NEW, customClassType.getInternalName());
		mv.visitInsn(DUP);
		mv.visitMethodInsn(INVOKESPECIAL, customClassType.getInternalName(), INIT, voidNoArgMethodDescriptor);
		mv.visitInsn(ARETURN);
		mv.visitMaxs(2, 1);
		mv.visitEnd();

		// plus original method signature
		mv = cw.visitMethod(ACC_PUBLIC + ACC_BRIDGE + ACC_SYNTHETIC, NEW_INSTANCE, NEW_INSTANCE_DESC, null, null);
		mv.visitCode();
		mv.visitVarInsn(ALOAD, 0);
		mv.visitMethodInsn(INVOKEVIRTUAL, className, NEW_INSTANCE, customTypeNoArgDesc);
		mv.visitInsn(ARETURN);
		mv.visitMaxs(1, 1);
		mv.visitEnd();

		// end class generation
		cw.visitEnd();

		return cw.toByteArray();
	}
}
