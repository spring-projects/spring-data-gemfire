/*
 * Copyright 2012 the original author or authors.
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
package org.springframework.data.gemfire.repository.support;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.when;

import java.io.Serializable;
import java.util.Collections;

import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.aop.framework.Advised;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.core.EntityInformation;

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

	@Before
	@SuppressWarnings("unchecked")
	public void setup() {
		when(region.getName()).thenReturn("simple");
		when(region.getFullPath()).thenReturn("/simple");
		when(region.getAttributes()).thenReturn(attributes);
	}

	/**
	 * @link https://jira.spring.io/browse/SGF-112
	 */
	@Test(expected = IllegalStateException.class)
	public void rejectsInterfacesExtendingPagingAndSortingRepository() {
		GemfireRepositoryFactory repositoryFactory = new GemfireRepositoryFactory(
			Collections.<Region<?, ?>>singletonList(region), new GemfireMappingContext());

		try {
			repositoryFactory.getRepository(SamplePagingAndSortingRepository.class);
			//factory.getRepository(SamplePagingRepository.class);
			//factory.getRepository(SampleSortingRepository.class);
		}
		catch (IllegalStateException expected) {
			assertThat(expected.getMessage(), Matchers.startsWith(
				"Pagination is not supported by Gemfire repositories!"));
			throw expected;
		}
	}

	@Test
	public void usesConfiguredRepositoryBaseClass() {
		GemfireRepositoryFactory repositoryFactory = new GemfireRepositoryFactory(
			Collections.<Region<?, ?>>singletonList(region), new GemfireMappingContext());

		repositoryFactory.setRepositoryBaseClass(CustomBaseRepository.class);

		GemfireRepository<?, ?> gemfireRepository = repositoryFactory.getRepository(SampleCustomGemfireRepository.class,
			new SampleCustomRepositoryImpl());

		assertSame(CustomBaseRepository.class, ((Advised) gemfireRepository).getTargetClass());
	}

	interface SamplePagingAndSortingRepository extends PagingAndSortingRepository<Person, Long> {
	}

	interface SamplePagingRepository extends Repository<Person, Long> {
		Page<Person> findAll(Pageable pageable);
	}

	interface SampleSortingRepository extends Repository<Person, Long> {
		Iterable<Person> findAll(Sort sort);
	}

	interface SampleCustomRepository<T> {
		void doCustomUpdate(T entity);
	}

	class SampleCustomRepositoryImpl<T> implements SampleCustomRepository<T> {

		@Override
		public void doCustomUpdate(final T entity) {
			throw new UnsupportedOperationException("Not Implemented!");
		}
	}

	interface SampleCustomGemfireRepository extends GemfireRepository<Person, Long>, SampleCustomRepository<Person> {
	}

	static class CustomBaseRepository<T, ID extends Serializable> extends SimpleGemfireRepository<T, ID> {

		public CustomBaseRepository(GemfireTemplate template, EntityInformation<T, ID> entityInformation) {
			super(template, entityInformation);
		}
	}

}
