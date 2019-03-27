/*
 * Copyright 2016-2019 the original author or authors.
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

package org.springframework.data.gemfire.search.lucene.support;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import org.aopalliance.intercept.MethodInterceptor;
import org.apache.geode.pdx.PdxInstance;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link PdxInstanceMethodInterceptorFactory}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.aopalliance.intercept.MethodInterceptor
 * @see org.apache.geode.pdx.PdxInstance
 * @see org.springframework.data.gemfire.search.lucene.support.PdxInstanceMethodInterceptorFactory
 * @since 1.1.0
 */
@RunWith(MockitoJUnitRunner.class)
public class PdxInstanceMethodInterceptorFactoryUnitTests {

	@Mock
	private PdxInstance mockSource;

	@Test
	public void createMethodInterceptorIsSuccessful() {
		MethodInterceptor methodInterceptor =
			PdxInstanceMethodInterceptorFactory.INSTANCE.createMethodInterceptor(mockSource, Object.class);

		assertThat(methodInterceptor).isInstanceOf(PdxInstanceMethodInterceptor.class);
		assertThat(((PdxInstanceMethodInterceptor) methodInterceptor).getSource()).isSameAs(mockSource);
	}

	@Test
	public void supportsPdxInstances() {
		assertThat(PdxInstanceMethodInterceptorFactory.INSTANCE.supports(mockSource, Object.class)).isTrue();
	}

	@Test
	public void doesNotSupportNonPdxInstances() {
		assertThat(PdxInstanceMethodInterceptorFactory.INSTANCE.supports(new Object(), Object.class)).isFalse();
		assertThat(PdxInstanceMethodInterceptorFactory.INSTANCE.supports(Boolean.TRUE, Object.class)).isFalse();
		assertThat(PdxInstanceMethodInterceptorFactory.INSTANCE.supports('X', Object.class)).isFalse();
		assertThat(PdxInstanceMethodInterceptorFactory.INSTANCE.supports(2, Object.class)).isFalse();
		assertThat(PdxInstanceMethodInterceptorFactory.INSTANCE.supports(Math.PI, Object.class)).isFalse();
		assertThat(PdxInstanceMethodInterceptorFactory.INSTANCE.supports("test", Object.class)).isFalse();
	}
}
