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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newUnsupportedOperationException;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.aop.framework.Advised;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.util.RegionUtils;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.core.EntityInformation;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.core.support.RepositoryComposition;

/**
 * Unit tests for {@link GemfireRepositoryFactory}.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.repository.support.GemfireRepositoryFactory
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("rawtypes")
public class GemfireRepositoryFactoryUnitTests {

	private GemfireMappingContext mappingContext;

	@Mock
	private Region mockRegion;

	@Mock
	private RegionAttributes mockRegionAttributes;

	@Before
	@SuppressWarnings("unchecked")
	public void setup() {
		this.mappingContext = new GemfireMappingContext();
		configureMockRegion(this.mockRegion, "simple", Object.class, Object.class);
	}

	@SuppressWarnings("unchecked")
	private <K, V> Region<K, V> mockRegion(String name, Class<K> keyType, Class<V> valueType) {
		return configureMockRegion(this.mockRegion, name, keyType, valueType);
	}

	@SuppressWarnings("unchecked")
	private <K, V> Region<K, V> configureMockRegion(Region<K, V> mockRegion, String name,
			Class<K> keyType, Class<V> valueType) {

		when(mockRegion.getAttributes()).thenReturn(this.mockRegionAttributes);
		when(mockRegion.getFullPath()).thenReturn(RegionUtils.toRegionPath(name));
		when(mockRegion.getName()).thenReturn(name);
		when(this.mockRegionAttributes.getKeyConstraint()).thenReturn(keyType);
		when(this.mockRegionAttributes.getValueConstraint()).thenReturn(valueType);

		return mockRegion;
	}

	private RepositoryMetadata mockRepositoryMetadata(Class<?> domainType, Class<?> idType,
			Class<?> repositoryInterface) {

		RepositoryMetadata mockRepositoryMetadata = mock(RepositoryMetadata.class);

		when(mockRepositoryMetadata.getDomainType()).thenAnswer(invocation -> domainType);
		when(mockRepositoryMetadata.getIdType()).thenAnswer(invocation -> idType);
		when(mockRepositoryMetadata.getRepositoryInterface()).thenAnswer(invocation -> repositoryInterface);

		return mockRepositoryMetadata;
	}

	@Test
	@SuppressWarnings("unchecked")
	public void constructGemfireRepositoryFactorySuccessfully() {

		GemfireRepositoryFactory repositoryFactory =
			new GemfireRepositoryFactory(Collections.singletonList(this.mockRegion), this.mappingContext);

		assertThat(repositoryFactory).isNotNull();
		assertThat(repositoryFactory.getMappingContext()).isEqualTo(this.mappingContext);
		assertThat(repositoryFactory.getRegions()).contains(this.mockRegion);
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructGemfireRepositoryFactoryWithNullMappingContextThrowsIllegalArgumentException() {

		try {
			new GemfireRepositoryFactory(Collections.emptyList(), null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("MappingContext is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructGemfireRepositoryFactoryWithNullRegionsThrowsIllegalArgumentException() {

		try {
			new GemfireRepositoryFactory(null, this.mappingContext);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Regions are required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void getEntityRegionNameFromEntityRegionName() {

		GemfireRepositoryFactory repositoryFactory =
			new GemfireRepositoryFactory(Collections.emptyList(), this.mappingContext);

		GemfirePersistentEntity<?> mockPersistentEntity = mock(GemfirePersistentEntity.class);

		when(mockPersistentEntity.getRegionName()).thenReturn("MockRegionName");

		RepositoryMetadata mockRepositoryMetadata = mock(RepositoryMetadata.class);

		assertThat(repositoryFactory.getEntityRegionName(mockRepositoryMetadata, mockPersistentEntity))
			.isEqualTo("MockRegionName");

		verify(mockPersistentEntity, times(1)).getRegionName();
		verify(mockPersistentEntity, never()).getType();
		verifyZeroInteractions(mockRepositoryMetadata);
	}

	@Test
	public void getEntityRegionNameFromEntityDomainType() {

		GemfireRepositoryFactory repositoryFactory =
			new GemfireRepositoryFactory(Collections.emptyList(), this.mappingContext);

		GemfirePersistentEntity<?> mockPersistentEntity = mock(GemfirePersistentEntity.class);

		RepositoryMetadata mockRepositoryMetadata = mock(RepositoryMetadata.class);

		when(mockPersistentEntity.getRegionName()).thenReturn("  ");
		when(mockPersistentEntity.getType()).thenAnswer(invocation -> Person.class);

		assertThat(repositoryFactory.getEntityRegionName(mockRepositoryMetadata, mockPersistentEntity))
			.isEqualTo(Person.class.getSimpleName());

		verify(mockPersistentEntity, times(1)).getRegionName();
		verify(mockPersistentEntity, times(1)).getType();
		verifyZeroInteractions(mockRepositoryMetadata);
	}

	@Test
	public void getEntityRegionNameFromRepositoryMetadata() {

		GemfireRepositoryFactory repositoryFactory =
			new GemfireRepositoryFactory(Collections.emptyList(), this.mappingContext);

		GemfirePersistentEntity<?> mockPersistentEntity = mock(GemfirePersistentEntity.class);

		RepositoryMetadata mockRepositoryMetadata = mock(RepositoryMetadata.class);

		when(mockPersistentEntity.getRegionName()).thenReturn("  ");
		when(mockPersistentEntity.getType()).thenReturn(null);
		when(mockRepositoryMetadata.getDomainType()).thenAnswer(invocation -> Person.class);

		assertThat(repositoryFactory.getEntityRegionName(mockRepositoryMetadata, mockPersistentEntity))
			.isEqualTo(Person.class.getSimpleName());

		verify(mockPersistentEntity, times(1)).getRegionName();
		verify(mockPersistentEntity, times(1)).getType();
		verify(mockRepositoryMetadata, times(1)).getDomainType();
	}

	@Test
	public void getEntityRegionNameFromRepositoryMetadataWhenEntityIsNull() {

		GemfireRepositoryFactory repositoryFactory =
			new GemfireRepositoryFactory(Collections.emptyList(), this.mappingContext);

		RepositoryMetadata mockRepositoryMetadata = mock(RepositoryMetadata.class);

		when(mockRepositoryMetadata.getDomainType()).thenAnswer(invocation -> Person.class);

		assertThat(repositoryFactory.getEntityRegionName(mockRepositoryMetadata, null))
			.isEqualTo(Person.class.getSimpleName());

		verify(mockRepositoryMetadata, times(1)).getDomainType();
	}

	@Test
	public void getRepositoryRegionNameFromRegionAnnotatedRepositoryInterface() {

		GemfireRepositoryFactory gemfireRepositoryFactory =
			new GemfireRepositoryFactory(Collections.emptyList(), this.mappingContext);

		RepositoryMetadata mockRepositoryMetadata = mock(RepositoryMetadata.class);

		when(mockRepositoryMetadata.getRepositoryInterface()).thenAnswer(invocation -> PeopleRepository.class);

		assertThat(gemfireRepositoryFactory.getRepositoryRegionName(mockRepositoryMetadata).orElse(null))
			.isEqualTo("People");

		verify(mockRepositoryMetadata, times(1)).getRepositoryInterface();
	}

	@Test
	public void getRepositoryRegionNameFromRegionAnnotatedRepositoryInterfaceHavingNoValue() {

		GemfireRepositoryFactory gemfireRepositoryFactory =
			new GemfireRepositoryFactory(Collections.emptyList(), this.mappingContext);

		RepositoryMetadata mockRepositoryMetadata = mock(RepositoryMetadata.class);

		when(mockRepositoryMetadata.getRepositoryInterface()).thenAnswer(invocation -> NonQualifiedRegionAnnotatedRepository.class);

		assertThat(gemfireRepositoryFactory.getRepositoryRegionName(mockRepositoryMetadata).isPresent()).isFalse();

		verify(mockRepositoryMetadata, times(1)).getRepositoryInterface();
	}

	@Test
	public void getRepositoryRegionNameFromNonRegionAnnotatedRepositoryInterface() {

		GemfireRepositoryFactory gemfireRepositoryFactory =
			new GemfireRepositoryFactory(Collections.emptyList(), this.mappingContext);

		RepositoryMetadata mockRepositoryMetadata = mock(RepositoryMetadata.class);

		when(mockRepositoryMetadata.getRepositoryInterface())
			.thenAnswer(invocation -> TestGemfireRepository.class);

		assertThat(gemfireRepositoryFactory.getRepositoryRegionName(mockRepositoryMetadata).isPresent()).isFalse();

		verify(mockRepositoryMetadata, times(1)).getRepositoryInterface();
	}

	@Test
	public void resolveRegionWithRepositoryMetadataAndRegionNamePath() {

		RepositoryMetadata mockRepositoryMetadata =
			mockRepositoryMetadata(Person.class, Long.class, PeopleRepository.class);

		Region<?, ?> mockRegionOne = mockRegion("RegionOne", Person.class, Long.class);
		Region<?, ?> mockRegionTwo = mockRegion("RegionTwo", Object.class, Long.class);

		GemfireRepositoryFactory repositoryFactory =
			new GemfireRepositoryFactory(Arrays.asList(mockRegionOne, mockRegionTwo), this.mappingContext);

		assertThat(repositoryFactory.resolveRegion(mockRepositoryMetadata, mockRegionTwo.getName()))
			.isEqualTo(mockRegionTwo);

		assertThat(repositoryFactory.resolveRegion(mockRepositoryMetadata, mockRegionOne.getFullPath()))
			.isEqualTo(mockRegionOne);

		verifyZeroInteractions(mockRepositoryMetadata);
	}

	@Test(expected = IllegalStateException.class)
	public void resolveRegionWithRepositoryMetadataAndNonExistingRegionNamePath() {

		try {
			RepositoryMetadata mockRepositoryMetadata =
				mockRepositoryMetadata(Person.class, Long.class, PeopleRepository.class);

			GemfireRepositoryFactory repositoryFactory =
				new GemfireRepositoryFactory(Collections.emptyList(), this.mappingContext);

			repositoryFactory.resolveRegion(mockRepositoryMetadata, "Test");
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage(GemfireRepositoryFactory.REGION_NOT_FOUND,
				"Test", Person.class.getName());

			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	@SuppressWarnings("unchecked")
	public void newTemplateReturnsGemfireTemplateForPeopleRegion() {

		RepositoryMetadata mockRepositoryMetadata =
			mockRepositoryMetadata(Person.class, Long.class, PeopleRepository.class);

		Region<Long, Person> mockPeopleRegion = mockRegion("People", Long.class, Person.class);

		GemfireRepositoryFactory repositoryFactory =
			new GemfireRepositoryFactory(Arrays.asList(this.mockRegion, mockPeopleRegion), mappingContext);

		GemfireTemplate gemfireTemplate = repositoryFactory.newTemplate(mockRepositoryMetadata);

		assertThat(gemfireTemplate).isNotNull();
		assertThat(gemfireTemplate.getRegion()).isEqualTo(mockPeopleRegion);

		verify(mockPeopleRegion, times(1)).getAttributes();
		verify(this.mockRegionAttributes, times(1)).getKeyConstraint();
		verify(mockRepositoryMetadata, times(1)).getDomainType();
		verify(mockRepositoryMetadata, times(1)).getIdType();
		verify(mockRepositoryMetadata, times(1)).getRepositoryInterface();
		verifyNoMoreInteractions(mockRepositoryMetadata);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void newTemplateReturnsGemfireTemplateForSimpleRegion() {

		RepositoryMetadata mockRepositoryMetadata =
			mockRepositoryMetadata(Person.class, Long.class, TestGemfireRepository.class);

		GemfireRepositoryFactory gemfireRepositoryFactory =
			new GemfireRepositoryFactory(Collections.singleton(this.mockRegion), this.mappingContext);

		GemfireTemplate gemfireTemplate = gemfireRepositoryFactory.newTemplate(mockRepositoryMetadata);

		assertThat(gemfireTemplate).isNotNull();
		assertThat(gemfireTemplate.getRegion()).isEqualTo(this.mockRegion);

		verify(this.mockRegion, times(1)).getAttributes();
		verify(this.mockRegionAttributes, times(1)).getKeyConstraint();
		verify(mockRepositoryMetadata, times(1)).getDomainType();
		verify(mockRepositoryMetadata, times(1)).getIdType();
		verify(mockRepositoryMetadata, times(1)).getRepositoryInterface();
		verifyNoMoreInteractions(mockRepositoryMetadata);
	}

	@Test(expected = IllegalArgumentException.class)
	public void newTemplateWithIncompatibleRegionKeyTypeAndRepositoryIdTypeThrowsIllegalArgumentException() {

		RepositoryMetadata mockRepositoryMetadata =
			mockRepositoryMetadata(Person.class, Long.class, PeopleRepository.class);

		Region<Integer, Person> mockPeopleRegion = mockRegion("People", Integer.class, Person.class);

		GemfireRepositoryFactory gemfireRepositoryFactory =
			new GemfireRepositoryFactory(Collections.singleton(mockPeopleRegion), this.mappingContext);

		try {
			gemfireRepositoryFactory.newTemplate(mockRepositoryMetadata);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage(GemfireRepositoryFactory.REGION_REPOSITORY_ID_TYPE_MISMATCH,
				"/People", Integer.class.getName(), PeopleRepository.class.getName(), Long.class.getName());

			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(mockPeopleRegion, times(1)).getAttributes();
			verify(mockPeopleRegion, atLeastOnce()).getFullPath();
			verify(this.mockRegionAttributes, times(1)).getKeyConstraint();
			verify(mockRepositoryMetadata, times(1)).getDomainType();
			verify(mockRepositoryMetadata, times(1)).getIdType();
			verify(mockRepositoryMetadata, times(2)).getRepositoryInterface();
			verifyNoMoreInteractions(mockRepositoryMetadata);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void newTemplateWithIncompatibleRepositoryIdTypeAndEntityIdTypeThrowsIllegalArgumentException() {

		RepositoryMetadata mockRepositoryMetadata =
			mockRepositoryMetadata(Person.class, Integer.class, PeopleIntegerRepository.class);

		Region<String, Person> mockPeopleRegion = mockRegion("People", null, null);

		GemfireRepositoryFactory gemfireRepositoryFactory =
			new GemfireRepositoryFactory(Collections.singleton(mockPeopleRegion), this.mappingContext);

		try {
			gemfireRepositoryFactory.newTemplate(mockRepositoryMetadata);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage(GemfireRepositoryFactory.REPOSITORY_ENTITY_ID_TYPE_MISMATCH,
				PeopleIntegerRepository.class.getName(), Integer.class.getName(), Person.class.getName(),
					Long.class.getName());

			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(mockPeopleRegion, times(1)).getAttributes();
			verify(this.mockRegionAttributes, times(1)).getKeyConstraint();
			verify(mockRepositoryMetadata, times(1)).getDomainType();
			verify(mockRepositoryMetadata, times(1)).getIdType();
			verify(mockRepositoryMetadata, times(2)).getRepositoryInterface();
			verifyNoMoreInteractions(mockRepositoryMetadata);
		}
	}

	@Test(expected = IllegalStateException.class)
	@SuppressWarnings("unchecked")
	public void newTemplateWithNonExistingRegionThrowsIllegalStateException() {

		RepositoryMetadata mockRepositoryMetadata =
			mockRepositoryMetadata(Person.class, Long.class, PeopleRepository.class);

		GemfireRepositoryFactory gemfireRepositoryFactory =
			new GemfireRepositoryFactory(Collections.singleton(this.mockRegion), this.mappingContext);

		try {
			gemfireRepositoryFactory.newTemplate(mockRepositoryMetadata);
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage(GemfireRepositoryFactory.REGION_NOT_FOUND,
				"People", Person.class.getName(), PeopleRepository.class.getName());

			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(mockRepositoryMetadata, times(2)).getDomainType();
			verify(mockRepositoryMetadata, never()).getIdType();
			verify(mockRepositoryMetadata, times(2)).getRepositoryInterface();
		}
	}

	/**
	 * @link <a href="https://jira.spring.io/browse/SGF-112">Repositories should reject PagingAndSortingRepository and Pageable parameters</a>
	 */
	@Test(expected = IllegalStateException.class)
	@SuppressWarnings("unchecked")
	public void rejectsInterfacesExtendingPagingAndSortingRepository() {

		GemfireRepositoryFactory repositoryFactory =
			new GemfireRepositoryFactory(Collections.singletonList(this.mockRegion), new GemfireMappingContext());

		try {
			repositoryFactory.getRepository(SamplePagingAndSortingRepository.class);
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessageStartingWith("Pagination is not supported by GemFire Repositories");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	@SuppressWarnings("unchecked")
	public void usesConfiguredRepositoryBaseClass() {

		GemfireRepositoryFactory repositoryFactory =
			new GemfireRepositoryFactory(Collections.singletonList(this.mockRegion), this.mappingContext);

		repositoryFactory.setRepositoryBaseClass(TestCustomBaseRepository.class);

		GemfireRepository<?, ?> gemfireRepository =
			repositoryFactory.getRepository(TestGemfireRepository.class,
				RepositoryComposition.RepositoryFragments.just(new TestCustomRepositoryImpl()));

		assertThat(((Advised) gemfireRepository).getTargetClass()).isEqualTo(TestCustomBaseRepository.class);
	}

	interface SamplePagingAndSortingRepository extends PagingAndSortingRepository<Person, Long> {
	}

	static class TestCustomBaseRepository<T, ID extends Serializable> extends SimpleGemfireRepository<T, ID> {

		public TestCustomBaseRepository(GemfireTemplate template, EntityInformation<T, ID> entityInformation) {
			super(template, entityInformation);
		}
	}

	interface TestCustomRepository<T> {

		@SuppressWarnings("unused")
		void doCustomUpdate(T entity);

	}

	class TestCustomRepositoryImpl<T> implements TestCustomRepository<T> {

		@Override
		public void doCustomUpdate(T entity) {
			throw newUnsupportedOperationException("Not Implemented");
		}
	}

	interface TestGemfireRepository extends GemfireRepository<Person, Long>, TestCustomRepository<Person> { }

	@org.springframework.data.gemfire.mapping.annotation.Region("People")
	interface PeopleRepository extends GemfireRepository<Person, Long> { }

	@org.springframework.data.gemfire.mapping.annotation.Region("People")
	interface PeopleIntegerRepository extends GemfireRepository<Person, Integer> { }

	@org.springframework.data.gemfire.mapping.annotation.Region
	interface NonQualifiedRegionAnnotatedRepository extends GemfireRepository<Person, Long> { }

}
