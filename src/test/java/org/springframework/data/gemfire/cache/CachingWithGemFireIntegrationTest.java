/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Map;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

import org.apache.geode.cache.Region;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;

/**
 * The CachingWithGemFireIntegrationTest class is a test suite of test cases testing the contract and functionality
 * of Spring Framework's Cache Abstraction using Pivotal GemFireas a caching provider applied with SDG.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.cache.annotation.Cacheable
 * @see org.springframework.test.context.ActiveProfiles
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.5.1
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@ActiveProfiles("replica")
@SuppressWarnings("unused")
public class CachingWithGemFireIntegrationTest {

	@Autowired
	private NamedNumbersService namedNumbersService;

	@Resource(name = "NamedNumbersRegion")
	private Region<String, Integer> namedNumbersRegion;

	@Test(expected = NullPointerException.class)
	public void testRegionCacheHit() {
		assertNull(namedNumbersRegion.get("eleven"));
		assertFalse(namedNumbersRegion.containsKey("eleven"));

		namedNumbersRegion.put("eleven", 11);

		assertTrue(namedNumbersRegion.containsKey("eleven"));
		assertEquals(11, namedNumbersService.get("eleven").intValue());
		assertFalse(namedNumbersService.wasCacheMiss());

		try {
			namedNumbersRegion.put("eleven", null); // Pivotal GemFiredoes not accept null values on put(key, value)
		}
		finally {
			assertTrue(namedNumbersRegion.containsKey("eleven"));
			assertEquals(11, namedNumbersRegion.get("eleven").intValue());
			assertEquals(11, namedNumbersService.get("eleven").intValue());
			assertFalse(namedNumbersService.wasCacheMiss());
		}
	}

	@Test
	public void testRegionCaching() {
		assertFalse(namedNumbersService.wasCacheMiss());
		assertEquals(1, namedNumbersService.get("one").intValue());
		assertTrue(namedNumbersService.wasCacheMiss());
		assertEquals(1, namedNumbersService.get("one").intValue());
		assertFalse(namedNumbersService.wasCacheMiss());
		assertEquals(2, namedNumbersService.get("two").intValue());
		assertTrue(namedNumbersService.wasCacheMiss());
		assertEquals(2, namedNumbersService.get("two").intValue());
		assertFalse(namedNumbersService.wasCacheMiss());
		assertNull(namedNumbersService.get("twelve"));
		assertTrue(namedNumbersService.wasCacheMiss());
		assertNull(namedNumbersService.get("twelve"));
		assertTrue(namedNumbersService.wasCacheMiss());
	}

	public static class NamedNumbersService {

		private NamedNumbersInMemoryRepository namedNumbersRepo;

		public final void setNamedNumbersRepo(final NamedNumbersInMemoryRepository namedNumbersRepo) {
			Assert.notNull(namedNumbersRepo, "The 'NamedNumbers' Repository must not be null!");
			this.namedNumbersRepo = namedNumbersRepo;
		}

		protected NamedNumbersInMemoryRepository getNamedNumbersRepo() {
			Assert.state(namedNumbersRepo != null,
				"A reference to the 'NamedNumbers' Repository was not properly configured and initialized!");
			return namedNumbersRepo;
		}

		@Cacheable("NamedNumbersRegion")
		public Integer get(final String namedNumber) {
			return getNamedNumbersRepo().get(namedNumber);
		}

		public boolean wasCacheMiss() {
			return getNamedNumbersRepo().wasCacheMiss();
		}
	}

	public static class NamedNumbersInMemoryRepository {

		private volatile boolean cacheMiss;

		private Map<String, Integer> namedNumbers;

		@PostConstruct
		public void init() {
			getNamedNumbers();
		}

		public final void setNamedNumbers(final Map<String, Integer> namedNumbers) {
			Assert.notNull(namedNumbers, "The reference to the 'NamedNumbers' Map must not be null!");
			this.namedNumbers = namedNumbers;
		}

		protected Map<String, Integer> getNamedNumbers() {
			Assert.state(namedNumbers != null, "The 'NamedNumbers' Map was not properly configured and initialized!");
			return namedNumbers;
		}

		public Integer get(final String namedNumber) {
			this.cacheMiss = true;
			return namedNumbers.get(namedNumber);
		}

		public boolean wasCacheMiss() {
			boolean localCacheMiss = this.cacheMiss;
			this.cacheMiss = false;
			return localCacheMiss;
		}
	}
}
