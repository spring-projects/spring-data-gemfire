/*
 * Copyright 2012 the original author or authors.
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
package org.springframework.data.gemfire.repository.support;

import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.hamcrest.Matchers;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.Repository;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;

/**
 * Unit tests for {@link GemfireRepositoryFactory}.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.springframework.data.gemfire.repository.support.GemfireRepositoryFactory
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("unused")
public class GemfireRepositoryFactoryUnitTests {

	@Mock
	private Region<?, ?> region;

	@Mock
	@SuppressWarnings("rawtypes")
	private RegionAttributes attributes;

	/**
	 * @link https://jira.spring.io/browse/SGF-112
	 */
	@Test(expected = IllegalStateException.class)
	@SuppressWarnings("unchecked")
	public void rejectsInterfacesExtendingPagingAndSortingRepository() {

		when(region.getName()).thenReturn("simple");
		when(region.getAttributes()).thenReturn(attributes);

		List<Region<?, ?>> regions = new ArrayList<Region<?, ?>>();
		regions.add(region);

		GemfireMappingContext context = new GemfireMappingContext();

		GemfireRepositoryFactory factory = new GemfireRepositoryFactory(regions, context);

		try {
			factory.getRepository(SampleInterface.class);
			//factory.getRepository(SamplePagingInterface.class);
			//factory.getRepository(SampleSortingInterface.class);
		} catch (IllegalStateException expected) {
			assertThat(expected.getMessage(), Matchers.startsWith("Pagination is not supported by Gemfire repositories!"));
			throw expected;
		}
	}

	interface SampleInterface extends PagingAndSortingRepository<Person, Long> {
	}

	interface SamplePagingInterface extends Repository<Person, Long> {
		Page<Person> findAll(Pageable pageable);
	}

	interface SampleSortingInterface extends Repository<Person, Long> {
		Iterable<Person> findAll(Sort sort);
	}

}
