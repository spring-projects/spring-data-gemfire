/*
 * Copyright 2016-2020 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.search.lucene;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneService;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.data.gemfire.search.lucene.support.ProjectingLuceneAccessorSupport;
import org.springframework.data.projection.ProjectionFactory;
import org.springframework.data.projection.SpelAwareProxyProjectionFactory;

/**
 * Unit tests for {@link ProjectingLuceneAccessor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.search.lucene.ProjectingLuceneAccessor
 * @since 1.1.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ProjectingLuceneAccessorUnitTests {

	@Mock
	private BeanFactory mockBeanFactory;

	@Mock
	private GemFireCache mockCache;

	@Mock
	private ClassLoader mockClassLoader;

	@Mock
	private LuceneIndex mockLuceneIndex;

	@Mock
	private LuceneService mockLuceneService;

	// SUT
	private ProjectingLuceneAccessor projectingLuceneAccessor;

	@Mock
	private ProjectionFactory mockProjectionFactory;

	@Mock
	private Region<?, ?> mockRegion;

	@Before
	public void setup() {
		projectingLuceneAccessor = spy(new ProjectingLuceneAccessorSupport() {});

		doReturn(mockCache).when(projectingLuceneAccessor).resolveCache();
		doReturn(mockLuceneService).when(projectingLuceneAccessor).resolveLuceneService();
		doReturn("MockLuceneIndex").when(projectingLuceneAccessor).resolveIndexName();
		doReturn("/Example").when(projectingLuceneAccessor).resolveRegionPath();
	}

	@Test
	public void afterPropertiesSetInitializesTheProjectingLuceneAccessorCorrectly() throws Exception {
		doReturn(mockProjectionFactory).when(projectingLuceneAccessor).resolveProjectionFactory();

		projectingLuceneAccessor.afterPropertiesSet();

		assertThat(projectingLuceneAccessor.getProjectionFactory()).isSameAs(mockProjectionFactory);

		verify(projectingLuceneAccessor, times(1)).resolveProjectionFactory();
		verifyZeroInteractions(mockProjectionFactory);
	}

	@Test
	public void projectingLuceneAccessorIsInitializedCorrectly() {
		assertThat(projectingLuceneAccessor.getBeanClassLoader()).isNull();
		assertThat(projectingLuceneAccessor.getBeanFactory()).isNull();
		assertThat(projectingLuceneAccessor.getCache()).isNull();
		assertThat(projectingLuceneAccessor.getIndexName()).isNull();
		assertThat(projectingLuceneAccessor.getLuceneIndex()).isNull();
		assertThat(projectingLuceneAccessor.getLuceneService()).isNull();
		assertThat(projectingLuceneAccessor.getProjectionFactory()).isNull();
		assertThat(projectingLuceneAccessor.getRegion()).isNull();
		assertThat(projectingLuceneAccessor.getRegionPath()).isNull();

		projectingLuceneAccessor.setBeanClassLoader(mockClassLoader);
		projectingLuceneAccessor.setBeanFactory(mockBeanFactory);
		projectingLuceneAccessor.setCache(mockCache);
		projectingLuceneAccessor.setIndexName("TestLuceneIdx");
		projectingLuceneAccessor.setLuceneIndex(mockLuceneIndex);
		projectingLuceneAccessor.setLuceneService(mockLuceneService);
		projectingLuceneAccessor.setProjectionFactory(mockProjectionFactory);
		projectingLuceneAccessor.setRegion(mockRegion);
		projectingLuceneAccessor.setRegionPath("/Example");

		assertThat(projectingLuceneAccessor.getBeanClassLoader()).isEqualTo(mockClassLoader);
		assertThat(projectingLuceneAccessor.getBeanFactory()).isEqualTo(mockBeanFactory);
		assertThat(projectingLuceneAccessor.getCache()).isEqualTo(mockCache);
		assertThat(projectingLuceneAccessor.getIndexName()).isEqualTo("TestLuceneIdx");
		assertThat(projectingLuceneAccessor.getLuceneIndex()).isEqualTo(mockLuceneIndex);
		assertThat(projectingLuceneAccessor.getLuceneService()).isEqualTo(mockLuceneService);
		assertThat(projectingLuceneAccessor.getProjectionFactory()).isEqualTo(mockProjectionFactory);
		assertThat(projectingLuceneAccessor.getRegion()).isEqualTo(mockRegion);
		assertThat(projectingLuceneAccessor.getRegionPath()).isEqualTo("/Example");
	}

	@Test
	public void setThenGetProjectionFactoryIsCorrect() {
		assertThat(projectingLuceneAccessor.getProjectionFactory()).isNull();
		assertThat(projectingLuceneAccessor.setThenGetProjectionFactory(mockProjectionFactory))
			.isSameAs(mockProjectionFactory);
		assertThat(projectingLuceneAccessor.getProjectionFactory()).isSameAs(mockProjectionFactory);
	}

	@Test
	public void resolveProjectFactoryReturnsProvidedProjectFactory() {
		assertThat(projectingLuceneAccessor.getProjectionFactory()).isNull();

		projectingLuceneAccessor.setProjectionFactory(mockProjectionFactory);

		assertThat(projectingLuceneAccessor.getProjectionFactory()).isSameAs(mockProjectionFactory);
		assertThat(projectingLuceneAccessor.resolveProjectionFactory()).isSameAs(mockProjectionFactory);
	}

	@Test
	public void resolveProjectionFactoryCreatesNewProjectionFactory() {
		assertThat(projectingLuceneAccessor.getProjectionFactory()).isNull();

		ProjectionFactory projectionFactory = projectingLuceneAccessor.resolveProjectionFactory();

		assertThat(projectionFactory).isInstanceOf(SpelAwareProxyProjectionFactory.class);
		assertThat(projectingLuceneAccessor.getProjectionFactory()).isSameAs(projectionFactory);
	}
}
