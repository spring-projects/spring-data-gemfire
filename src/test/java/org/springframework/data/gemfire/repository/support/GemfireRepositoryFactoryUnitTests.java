/*
 * Copyright 2012-2018 the original author or authors.
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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.springframework.aop.framework.Advised;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.core.EntityInformation;
import org.springframework.data.repository.core.RepositoryMetadata;

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

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private GemfireMappingContext gemfireMappingContext = new GemfireMappingContext();

	@Mock
	private Region<Object, Object> mockRegion;

	@Mock
	@SuppressWarnings("rawtypes")
	private RegionAttributes mockRegionAttributes;

	@SuppressWarnings("unchecked")
	protected <K, V> Region<K, V> configureMockRegion(Region<K, V> mockRegion, String name,
			Class<K> keyType, Class<V> valueType) {

		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegion.getFullPath()).thenReturn(String.format("%1$s%2$s", Region.SEPARATOR, name));
		when(mockRegion.getName()).thenReturn(name);
		when(mockRegionAttributes.getKeyConstraint()).thenReturn(keyType);
		when(mockRegionAttributes.getValueConstraint()).thenReturn(valueType);

		return mockRegion;
	}

	@SuppressWarnings("unchecked")
	protected <K, V> Region<K, V> mockRegion(String name, Class<K> keyType, Class<V> valueType) {
		return configureMockRegion(mock(Region.class, name), name, keyType, valueType);
	}

	protected RepositoryMetadata mockRepositoryMetadata(final Class<?> domainType, final Class<?> idType,
			final Class<?> repositoryInterface) {

		RepositoryMetadata mockRepositoryMetadata = mock(RepositoryMetadata.class);

		when(mockRepositoryMetadata.getDomainType()).then(new Answer<Class<?>>() {
			@Override public Class<?> answer(InvocationOnMock invocation) throws Throwable {
				return domainType;
			}
		});

		when(mockRepositoryMetadata.getIdType()).then(new Answer<Class<?>>() {
			@Override public Class<?> answer(InvocationOnMock invocation) throws Throwable {
				return idType;
			}
		});

		when(mockRepositoryMetadata.getRepositoryInterface()).then(new Answer<Class<?>>() {
			@Override public Class<?> answer(InvocationOnMock invocation) throws Throwable {
				return repositoryInterface;
			}
		});

		return mockRepositoryMetadata;
	}

	@Before
	@SuppressWarnings("unchecked")
	public void setup() {
		configureMockRegion(mockRegion, "simple", Object.class, Object.class);
	}

	@Test
	public void constructGemfireRepositoryFactoryWithNullMappingContextThrowsIllegalArgumentException() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("MappingContext must not be null");

		new GemfireRepositoryFactory(Collections.<Region<?, ?>>emptyList(), null);
	}

	@Test
	public void constructGemfireRepositoryFactoryWithNullRegionsThrowsIllegalArgumentException() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("Regions must not be null");

		new GemfireRepositoryFactory(null, gemfireMappingContext);
	}

	@Test
	public void getRepositoryRegionNameFromRepositoryInterfaceWithRegionAnnotation() {
		GemfireRepositoryFactory gemfireRepositoryFactory = new GemfireRepositoryFactory(
			Collections.<Region<?, ?>>emptyList(), gemfireMappingContext);

		assertThat(gemfireRepositoryFactory.getRepositoryRegionName(PersonRepository.class),
			is(equalTo("People")));
	}

	@Test
	public void getRepositoryRegionNameFromRepositoryInterfaceWithoutRegionAnnotation() {
		GemfireRepositoryFactory gemfireRepositoryFactory = new GemfireRepositoryFactory(
			Collections.<Region<?, ?>>emptyList(), gemfireMappingContext);

		assertThat(gemfireRepositoryFactory.getRepositoryRegionName(SampleCustomGemfireRepository.class),
			is(nullValue(String.class)));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getTemplateReturnsGemfireTemplateForPeopleRegion() {
		RepositoryMetadata mockRepositoryMetadata = mockRepositoryMetadata(Person.class, Long.class,
			PersonRepository.class);

		Region<Long, Person> mockPeopleRegion = mockRegion("People", Long.class, Person.class);

		Iterable<Region<?, ?>> regions = Arrays.asList(mockRegion, mockPeopleRegion);

		GemfireRepositoryFactory gemfireRepositoryFactory = new GemfireRepositoryFactory(
			regions, gemfireMappingContext);

		GemfireTemplate gemfireTemplate = gemfireRepositoryFactory.getTemplate(mockRepositoryMetadata);

		assertThat(gemfireTemplate, is(notNullValue(GemfireTemplate.class)));
		assertThat(gemfireTemplate.<Long, Person>getRegion(), is(equalTo(mockPeopleRegion)));

		verify(mockPeopleRegion, times(1)).getAttributes();
		verify(mockPeopleRegion, times(1)).getFullPath();
		verify(mockPeopleRegion, times(1)).getName();
		verify(mockRegionAttributes, times(1)).getKeyConstraint();
		verify(mockRepositoryMetadata, times(1)).getDomainType();
		verify(mockRepositoryMetadata, times(1)).getIdType();
	}

	@Test
	public void getTemplateReturnsGemfireTemplateForSimpleRegion() {
		RepositoryMetadata mockRepositoryMetadata = mockRepositoryMetadata(Person.class, Long.class,
			SampleCustomGemfireRepository.class);

		Iterable<Region<?, ?>> regions = Collections.<Region<?, ?>>singleton(mockRegion);

		GemfireRepositoryFactory gemfireRepositoryFactory = new GemfireRepositoryFactory(
			regions, gemfireMappingContext);

		GemfireTemplate gemfireTemplate = gemfireRepositoryFactory.getTemplate(mockRepositoryMetadata);

		assertThat(gemfireTemplate, is(notNullValue(GemfireTemplate.class)));
		assertThat(gemfireTemplate.getRegion(), is(equalTo(mockRegion)));

		verify(mockRepositoryMetadata, times(1)).getDomainType();
		verify(mockRepositoryMetadata, times(1)).getIdType();
	}

	@Test
	public void getTemplateThrowsIllegalArgumentExceptionForIncompatibleRegionKeyTypeAndRepositoryIdType() {
		RepositoryMetadata mockRepositoryMetadata = mockRepositoryMetadata(Person.class, Long.class,
			PersonRepository.class);

		Region<String, Person> mockPeopleRegion = mockRegion("People", String.class, Person.class);

		GemfireRepositoryFactory gemfireRepositoryFactory = new GemfireRepositoryFactory(
			Collections.<Region<?, ?>>singleton(mockPeopleRegion), gemfireMappingContext);

		try {
			exception.expect(IllegalArgumentException.class);
			exception.expectCause(is(nullValue(Throwable.class)));
			exception.expectMessage(String.format(
				"The Region referenced only supports keys of type [%1$s], but the entity to be stored has an id of type [%2$s]",
					String.class.getName(), Long.class.getName()));

			gemfireRepositoryFactory.getTemplate(mockRepositoryMetadata);
		}
		finally {
			verify(mockRepositoryMetadata, times(1)).getDomainType();
			verify(mockRepositoryMetadata, times(1)).getIdType();
			verify(mockPeopleRegion, times(1)).getAttributes();
			verify(mockPeopleRegion, times(1)).getFullPath();
			verify(mockPeopleRegion, times(1)).getName();
			verify(mockRegionAttributes, times(1)).getKeyConstraint();
		}
	}

	@Test
	public void getTemplateThrowsIllegalStateExceptionForRegionNotFound() {
		RepositoryMetadata mockRepositoryMetadata = mockRepositoryMetadata(Person.class, Long.class,
			PersonRepository.class);

		GemfireRepositoryFactory gemfireRepositoryFactory = new GemfireRepositoryFactory(
			Collections.<Region<?, ?>>singleton(mockRegion), gemfireMappingContext);

		try {
			exception.expect(IllegalStateException.class);
			exception.expectCause(is(nullValue(Throwable.class)));
			exception.expectMessage(String.format(
				"No Region [People] was found for domain class [%s]; Make sure you have configured a GemFire Region of that name in your application context",
					Person.class.getName()));

			gemfireRepositoryFactory.getTemplate(mockRepositoryMetadata);
		}
		finally {
			verify(mockRepositoryMetadata, times(2)).getDomainType();
			verify(mockRepositoryMetadata, never()).getIdType();
			verify(mockRegion, times(1)).getFullPath();
			verify(mockRegion, times(1)).getName();
			verifyZeroInteractions(mockRegionAttributes);
		}
	}

	/**
	 * @link https://jira.spring.io/browse/SGF-112
	 */
	@Test
	public void rejectsInterfacesExtendingPagingAndSortingRepository() {
		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(startsWith("Pagination is not supported by GemFire Repositories"));

		GemfireRepositoryFactory repositoryFactory = new GemfireRepositoryFactory(
			Collections.<Region<?, ?>>singletonList(mockRegion), new GemfireMappingContext());

		repositoryFactory.getRepository(SamplePagingAndSortingRepository.class);
	}

	@Test
	public void usesConfiguredRepositoryBaseClass() {
		GemfireRepositoryFactory repositoryFactory = new GemfireRepositoryFactory(
			Collections.<Region<?, ?>>singletonList(mockRegion), new GemfireMappingContext());

		repositoryFactory.setRepositoryBaseClass(CustomBaseRepository.class);

		GemfireRepository<?, ?> gemfireRepository = repositoryFactory.getRepository(SampleCustomGemfireRepository.class,
			new SampleCustomRepositoryImpl());

		assertThat(((Advised) gemfireRepository).getTargetClass(), is(equalTo((Class) CustomBaseRepository.class)));
	}

	interface SamplePagingAndSortingRepository extends PagingAndSortingRepository<Person, Long> {
	}

	static class CustomBaseRepository<T, ID extends Serializable> extends SimpleGemfireRepository<T, ID> {

		public CustomBaseRepository(GemfireTemplate template, EntityInformation<T, ID> entityInformation) {
			super(template, entityInformation);
		}
	}

	interface SampleCustomRepository<T> {
		void doCustomUpdate(T entity);
	}

	class SampleCustomRepositoryImpl<T> implements SampleCustomRepository<T> {
		@Override
		public void doCustomUpdate(final T entity) {
			throw new UnsupportedOperationException("Not Implemented");
		}
	}

	interface SampleCustomGemfireRepository extends GemfireRepository<Person, Long>, SampleCustomRepository<Person> {
	}

	@org.springframework.data.gemfire.mapping.annotation.Region("People")
	interface PersonRepository extends GemfireRepository<Person, Long> {
	}
}
