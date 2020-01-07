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

import static org.assertj.core.api.Java6Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.lucene.LuceneService;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link LuceneServiceFactoryBean}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.search.lucene.LuceneServiceFactoryBean
 * @since 1.1.0
 */
@RunWith(MockitoJUnitRunner.class)
public class LuceneServiceFactoryBeanUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Mock
	private GemFireCache mockCache;

	@Mock
	private LuceneService mockLuceneService;

	private LuceneServiceFactoryBean factoryBean;

	@Before
	public void setup() {
		factoryBean = spy(new LuceneServiceFactoryBean());
		doReturn(mockLuceneService).when(factoryBean).resolveLuceneService(eq(mockCache));
	}

	@Test
	public void setAndGetCache() {
		assertThat(factoryBean.getCache()).isNull();

		factoryBean.setCache(mockCache);

		assertThat(factoryBean.getCache()).isSameAs(mockCache);

		factoryBean.setCache(null);

		assertThat(factoryBean.getCache()).isNull();
	}

	@Test
	public void afterPropertiesSetInitializesLuceneService() throws Exception {
		assertThat(factoryBean.getObject()).isNull();

		factoryBean.setCache(mockCache);
		factoryBean.afterPropertiesSet();

		assertThat(factoryBean.getObject()).isEqualTo(mockLuceneService);

		verify(factoryBean, times(1)).resolveLuceneService(eq(mockCache));
	}

	@Test
	public void afterPropertiesSetThrowsIllegalStateExceptionWhenGemFireCacheIsNull() throws Exception {
		assertThat(factoryBean.getCache()).isNull();

		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("A reference to the GemFireCache was not properly configured");

		factoryBean.afterPropertiesSet();
	}

	@Test
	public void getObjectTypeBeforeInitialization() throws Exception {
		assertThat(factoryBean.getObject()).isNull();
		assertThat(factoryBean.getObjectType()).isEqualTo(LuceneService.class);
	}

	@Test
	public void getObjectTypeAfterInitialization() throws Exception {
		factoryBean.setCache(mockCache);
		factoryBean.afterPropertiesSet();

		assertThat(factoryBean.getObject()).isSameAs(mockLuceneService);
		assertThat(factoryBean.getObjectType()).isEqualTo(mockLuceneService.getClass());
	}

	@Test
	public void isSingletonReturnsTrue() {
		assertThat(factoryBean.isSingleton()).isTrue();
	}
}
