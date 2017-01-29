/*
 * Copyright 2010-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.repository.sample;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * The AlgorithmRepositoryTest class is a test suite of test cases testing the contract and functionality of GemFire's
 * Repository extension when using a plain old Java interface for defining the application domain object/entity type,
 * rather than a Java class, that is the subject of the persistence operations.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.4.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class AlgorithmRepositoryTest {

	@Autowired
	private AlgorithmRepository algorithmRepo;

	@Resource(name = "Algorithms")
	private Region algorithmsRegion;

	@Test
	public void algorithmsRepositoryFunctionsCorrectly() {
		assertNotNull("A reference to the AlgorithmRepository was not properly configured!", algorithmRepo);
		assertNotNull("A reference to the 'Algorithms' GemFire Cache Region was not properly configured!",
			algorithmsRegion);
		assertEquals("Algorithms", algorithmsRegion.getName());
		assertEquals("/Algorithms", algorithmsRegion.getFullPath());
		assertTrue(algorithmsRegion.isEmpty());

		algorithmRepo.save(new BinarySearch());
		algorithmRepo.save(new HeapSort());

		assertFalse(algorithmsRegion.isEmpty());
		assertEquals(2, algorithmsRegion.size());

		assertTrue(algorithmsRegion.get(BinarySearch.class.getSimpleName()) instanceof BinarySearch);
		assertTrue(algorithmsRegion.get(HeapSort.class.getSimpleName()) instanceof HeapSort);

		HeapSort heapSort = algorithmRepo.findByName(HeapSort.class.getSimpleName());

		assertNotNull(heapSort);
		assertEquals(HeapSort.class.getSimpleName(), heapSort.getName());

		BinarySearch binarySearch = algorithmRepo.findByName(BinarySearch.class.getSimpleName());

		assertNotNull(binarySearch);
		assertEquals(BinarySearch.class.getSimpleName(), binarySearch.getName());
	}

	protected static abstract class AbstractAlgorithm implements Algorithm {

		@Override
		public String getName() {
			return getClass().getSimpleName();
		}
	}

	protected static final class BinarySearch extends AbstractAlgorithm {
	}

	protected static final class HeapSort extends AbstractAlgorithm {
	}
}
