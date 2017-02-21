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
 *
 */

package org.springframework.data.gemfire.repository.cdi;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isIn;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.annotation.Annotation;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.enterprise.context.spi.CreationalContext;
import javax.enterprise.inject.spi.Bean;
import javax.enterprise.inject.spi.BeanManager;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.data.gemfire.GemfireAccessor;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactory;
import org.springframework.data.gemfire.repository.support.SimpleGemfireRepository;

/**
 * The GemfireRepositoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the GemfireRepositoryBean class.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.repository.cdi.GemfireRepositoryBean
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("unchecked")
public class GemfireRepositoryBeanTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Mock
	private BeanManager mockBeanManager;

	protected <T> T[] asArray(T... array) {
		return array;
	}

	protected <T> Set<T> asSet(T... array) {
		return new HashSet<T>(Arrays.asList(array));
	}

	protected <T> Set<T> asSet(Iterable<T> collection) {
		Set<T> set = new HashSet<T>();

		for (T element : collection) {
			set.add(element);
		}

		return set;
	}

	@Test
	public void getDependencyInstanceGetsReference() {
		Bean<Region> mockRegionBean = mock(Bean.class);
		CreationalContext<Region> mockCreationalContext = mock(CreationalContext.class);
		Region mockRegion = mock(Region.class);

		when(mockBeanManager.createCreationalContext(eq(mockRegionBean))).thenReturn(mockCreationalContext);
		when(mockBeanManager.getReference(eq(mockRegionBean), eq(Region.class), eq(mockCreationalContext)))
			.thenReturn(mockRegion);

		GemfireRepositoryBean<PersonRepository> repositoryBean = new GemfireRepositoryBean<PersonRepository>(
			mockBeanManager, PersonRepository.class, Collections.<Annotation>emptySet(), null, null, null);

		assertThat(repositoryBean.getDependencyInstance(mockRegionBean, Region.class), is(equalTo(mockRegion)));

		verify(mockBeanManager, times(1)).createCreationalContext(eq(mockRegionBean));
		verify(mockBeanManager, times(1)).getReference(eq(mockRegionBean), eq(Region.class), eq(mockCreationalContext));
	}

	@Test
	public void resolveGemfireMappingContextUsesDefault() {
		GemfireRepositoryBean<PersonRepository> repositoryBean = new GemfireRepositoryBean<PersonRepository>(
			mockBeanManager, PersonRepository.class, Collections.<Annotation>emptySet(), null, null, null);

		assertThat(repositoryBean.resolveGemfireMappingContext(),
			is(equalTo(GemfireRepositoryBean.DEFAULT_GEMFIRE_MAPPING_CONTEXT)));
	}

	@Test
	public void resolveGemfireMappingContextUsesQualifiedMappingContext() {
		Bean<GemfireMappingContext> mockMappingContextBean = mock(Bean.class);
		CreationalContext<GemfireMappingContext> mockCreationalContext = mock(CreationalContext.class);
		GemfireMappingContext expectedGemfireMappingContext = new GemfireMappingContext();

		when(mockBeanManager.createCreationalContext(eq(mockMappingContextBean))).thenReturn(mockCreationalContext);
		when(mockBeanManager.getReference(eq(mockMappingContextBean), eq(GemfireMappingContext.class),
			eq(mockCreationalContext))).thenReturn(expectedGemfireMappingContext);

		GemfireRepositoryBean<PersonRepository> repositoryBean = new GemfireRepositoryBean<PersonRepository>(
			mockBeanManager, PersonRepository.class, Collections.<Annotation>emptySet(), null,
				mockMappingContextBean, null);

		GemfireMappingContext actualGemfireMappingContext = repositoryBean.resolveGemfireMappingContext();

		assertThat(actualGemfireMappingContext, is(equalTo(expectedGemfireMappingContext)));

		verify(mockBeanManager, times(1)).createCreationalContext(eq(mockMappingContextBean));
		verify(mockBeanManager, times(1)).getReference(eq(mockMappingContextBean), eq(GemfireMappingContext.class),
			eq(mockCreationalContext));
	}

	@Test
	public void resolveGemfireRegions() {
		Region mockRegionOne = mock(Region.class);
		Region mockRegionTwo = mock(Region.class);

		CreationalContext<Bean<Region>> mockCreationalContext = mock(CreationalContext.class);

		Bean<Region> mockRegionBeanOne = mock(Bean.class);
		Bean<Region> mockRegionBeanTwo = mock(Bean.class);

		when(mockRegionBeanOne.getTypes()).thenReturn(asSet((Type) Region.class));
		when(mockRegionBeanTwo.getTypes()).thenReturn(asSet((Type) Region.class));
		when(mockBeanManager.createCreationalContext(any(Bean.class))).thenReturn(mockCreationalContext);
		when(mockBeanManager.getReference(eq(mockRegionBeanOne), eq(Region.class), eq(mockCreationalContext)))
			.thenReturn(mockRegionOne);
		when(mockBeanManager.getReference(eq(mockRegionBeanTwo), eq(Region.class), eq(mockCreationalContext)))
			.thenReturn(mockRegionTwo);

		GemfireRepositoryBean repositoryBean = new GemfireRepositoryBean(mockBeanManager,
			PersonRepository.class, Collections.emptySet(), null, null, asSet(mockRegionBeanOne, mockRegionBeanTwo));

		Iterable<Region> regions = repositoryBean.resolveGemfireRegions();

		assertThat(regions, is(notNullValue()));
		assertThat(asSet(regions).containsAll(asSet(mockRegionOne, mockRegionTwo)), is(true));

		verify(mockRegionBeanOne, times(1)).getTypes();
		verify(mockRegionBeanTwo, times(1)).getTypes();
		verify(mockBeanManager, times(1)).createCreationalContext(eq(mockRegionBeanOne));
		verify(mockBeanManager, times(1)).createCreationalContext(eq(mockRegionBeanTwo));
		verify(mockBeanManager, times(1)).getReference(eq(mockRegionBeanOne), eq(Region.class),
			eq(mockCreationalContext));
		verify(mockBeanManager, times(1)).getReference(eq(mockRegionBeanTwo), eq(Region.class),
			eq(mockCreationalContext));
	}

	@Test
	public void resolveTypeFindsTargetComponentType() {
		Bean mockBean = mock(Bean.class);

		when(mockBean.getTypes()).thenReturn(
			asSet((Type) Object.class, Map.class, ConcurrentMap.class, Region.class));

		GemfireRepositoryBean<PersonRepository> repositoryBean = new GemfireRepositoryBean<PersonRepository>(
			mockBeanManager, PersonRepository.class, Collections.<Annotation>emptySet(), null, null, null);

		assertThat(repositoryBean.resolveType(mockBean, Region.class), is(equalTo((Type) Region.class)));
		assertThat(repositoryBean.resolveType(mockBean, Map.class), isIn(asArray((Type) Map.class,
			ConcurrentMap.class, Region.class)));

		verify(mockBean, times(2)).getTypes();
	}

	@Test
	public void resolveTypeWithParameterizedType() {
		Bean<Map> mockBean = mock(Bean.class);
		Map<Long, Object> parameterizedTypeMap = Collections.emptyMap();
		ParameterizedType mockParameterizedType = mock(ParameterizedType.class);

		assertThat(parameterizedTypeMap.getClass(), is(instanceOf(Type.class)));
		assertThat(parameterizedTypeMap.getClass().getGenericSuperclass(), is(instanceOf(ParameterizedType.class)));
		assertThat(parameterizedTypeMap.getClass().getTypeParameters().length, is(equalTo(2)));

		when(mockBean.getTypes()).thenReturn(asSet((Type) mockParameterizedType));
		when(mockParameterizedType.getRawType()).thenReturn(parameterizedTypeMap.getClass());

		GemfireRepositoryBean<PersonRepository> repositoryBean = new GemfireRepositoryBean<PersonRepository>(
			mockBeanManager, PersonRepository.class, Collections.<Annotation>emptySet(), null, null, null);

		assertThat(repositoryBean.resolveType(mockBean, Map.class), is(equalTo((Type) mockParameterizedType)));

		verify(mockBean, times(1)).getTypes();
		verify(mockParameterizedType, times(1)).getRawType();
	}

	@Test
	public void resolveTypeWithUnresolvableType() {
		Bean mockBean = mock(Bean.class);

		when(mockBean.getTypes()).thenReturn(asSet((Type) Map.class, Object.class));

		GemfireRepositoryBean<PersonRepository> repositoryBean = new GemfireRepositoryBean<PersonRepository>(
			mockBeanManager, PersonRepository.class, Collections.<Annotation>emptySet(), null, null, null);

		try {
			expectedException.expect(IllegalStateException.class);
			expectedException.expectCause(is(nullValue(Throwable.class)));
			expectedException.expectMessage(is(equalTo(String.format(
				"unable to resolve bean instance of type [%1$s] from bean definition [%2$s]",
					Region.class, mockBean))));

			repositoryBean.resolveType(mockBean, Region.class);
		}
		finally {
			verify(mockBean, times(1)).getTypes();
		}
	}

	@Test
	// IntegrationTest
	public void createGemfireRepositoryInstanceSuccessfully() throws Exception {
		Bean<Region> mockRegionBean = mock(Bean.class);

		CreationalContext<Bean<Region>> mockCreationalContext = mock(CreationalContext.class);

		final Region mockRegion = mock(Region.class);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegion.getName()).thenReturn("Person");
		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getKeyConstraint()).thenReturn(Long.class);
		when(mockRegionBean.getTypes()).thenReturn(asSet((Type) Region.class));
		when(mockBeanManager.createCreationalContext(any(Bean.class))).thenReturn(mockCreationalContext);
		when(mockBeanManager.getReference(eq(mockRegionBean), eq(Region.class), eq(mockCreationalContext)))
			.thenReturn(mockRegion);

		final AtomicBoolean repositoryProxyPostProcessed = new AtomicBoolean(false);

		GemfireRepositoryBean<PersonRepository> repositoryBean = new GemfireRepositoryBean<PersonRepository>(
			mockBeanManager, PersonRepository.class, Collections.<Annotation>emptySet(), null, null,
				asSet(mockRegionBean))
		{
			@Override
			GemfireRepositoryFactory newGemfireRepositoryFactory() {
				GemfireRepositoryFactory gemfireRepositoryFactory = super.newGemfireRepositoryFactory();

				gemfireRepositoryFactory.addRepositoryProxyPostProcessor((factory, repositoryInformation) -> {
					try {
						assertThat(repositoryInformation.getRepositoryInterface(),
							is(equalTo(PersonRepository.class)));
						assertThat(repositoryInformation.getRepositoryBaseClass(),
							is(equalTo(SimpleGemfireRepository.class)));
						assertThat(repositoryInformation.getDomainType(), is(equalTo(Person.class)));
						assertThat(repositoryInformation.getIdType(), is(equalTo(Long.class)));
						assertThat(factory.getTargetClass(), is(equalTo(SimpleGemfireRepository.class)));

						Object gemfireRepository = factory.getTargetSource().getTarget();

						GemfireAccessor gemfireAccessor = TestUtils.readField("template", gemfireRepository);

						assertThat(gemfireAccessor, is(notNullValue()));
						assertThat(gemfireAccessor.getRegion(), is(equalTo(mockRegion)));

						repositoryProxyPostProcessed.set(true);
					}
					catch (Exception e) {
						throw new RuntimeException(e);
					}
				});
				return gemfireRepositoryFactory;
			}
		};

		GemfireRepository<Person, Long> gemfireRepository =
			repositoryBean.create(null, PersonRepository.class, Optional.empty());

		assertThat(gemfireRepository, is(notNullValue()));
		assertThat(repositoryProxyPostProcessed.get(), is(true));

		verify(mockBeanManager, times(1)).createCreationalContext(eq(mockRegionBean));
		verify(mockBeanManager, times(1)).getReference(eq(mockRegionBean), eq(Region.class),
			eq(mockCreationalContext));
		verify(mockRegionBean, times(1)).getTypes();
		verify(mockRegion, times(1)).getName();
		verify(mockRegion, times(1)).getAttributes();
		verify(mockRegionAttributes, times(1)).getKeyConstraint();
	}

	class TestMap extends AbstractMap<Long, Object> {
		@Override public Set<Entry<Long, Object>> entrySet() {
			return Collections.emptySet();
		}
	}

	class Person {}

	interface PersonRepository extends GemfireRepository<Person, Long> {
	}

}
