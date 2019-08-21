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

import static org.springframework.data.gemfire.search.lucene.support.PdxInstanceMethodInterceptor.newPdxInstanceMethodInterceptor;

import org.apache.geode.pdx.PdxInstance;

import org.aopalliance.intercept.MethodInterceptor;

import org.springframework.data.projection.MethodInterceptorFactory;

/**
 * The {@link PdxInstanceMethodInterceptorFactory} class is a Spring Data {@link MethodInterceptorFactory} used to
 * identify {@link PdxInstance} types and instantiates an instance of the {@link PdxInstanceMethodInterceptor}
 * in order to intercept and handle invocations on the {@link PdxInstance} for the proxied projection.
 *
 * @author John Blum
 * @see org.aopalliance.intercept.MethodInterceptor
 * @see org.apache.geode.pdx.PdxInstance
 * @see org.springframework.data.gemfire.search.lucene.support.PdxInstanceMethodInterceptor
 * @see org.springframework.data.projection.MethodInterceptorFactory
 * @since 1.0.0
 */
public enum PdxInstanceMethodInterceptorFactory implements MethodInterceptorFactory {

	INSTANCE;

	/**
	 * @inheritDoc
	 */
	@Override
	public MethodInterceptor createMethodInterceptor(Object source, Class<?> targetType) {
		return newPdxInstanceMethodInterceptor(source);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean supports(Object source, Class<?> targetType) {
		return PdxInstance.class.isInstance(source);
	}
}
