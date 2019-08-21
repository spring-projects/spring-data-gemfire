/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.serialization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.Serializable;

import org.apache.geode.DataSerializable;
import org.apache.geode.Instantiator;

import org.junit.Before;
import org.junit.Test;

/**
 * @author Costin Leau
 */
public class AsmInstantiatorFactoryTest {

	@SuppressWarnings("serial")
	public static class SomeClass implements DataSerializable {
		public static boolean instantiated = false;

		public SomeClass() {
			instantiated = true;
		}

		public void fromData(DataInput in) throws IOException, ClassNotFoundException {
		}

		public void toData(DataOutput out) throws IOException {
		}
	}

	private AsmInstantiatorGenerator asmFactory = null;

	@Before
	public void setUp() {
		SomeClass.instantiated = false;
		asmFactory = new AsmInstantiatorGenerator();
	}

	@Test
	public void testClassGeneration() throws Exception {
		Instantiator instantiator = asmFactory.getInstantiator(SomeClass.class, 100);
		assertEquals(100, instantiator.getId());
		assertEquals(SomeClass.class, instantiator.getInstantiatedClass());
		Object instance = instantiator.newInstance();
		assertEquals(SomeClass.class, instance.getClass());
		assertTrue(SomeClass.instantiated);
	}

	@Test
	public void testGeneratedClassName() throws Exception {
		Instantiator instantiator = asmFactory.getInstantiator(SomeClass.class, 100);
		assertTrue(instantiator.getClass().getName().contains("$"));
	}

	@Test
	public void testInterfaces() throws Exception {
		Instantiator instantiator = asmFactory.getInstantiator(SomeClass.class, 100);
		assertTrue(instantiator instanceof Serializable);
	}

	@Test
	public void testCacheInPlace() throws Exception {
		Instantiator instance1 = asmFactory.getInstantiator(SomeClass.class, 120);
		Instantiator instance2 = asmFactory.getInstantiator(SomeClass.class, 125);
		assertSame(instance1, instance2);
	}
}
