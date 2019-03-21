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

import java.lang.reflect.Method;
import java.util.Arrays;

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.geode.pdx.PdxInstance;
import org.apache.geode.pdx.WritablePdxInstance;
import org.springframework.data.projection.Accessor;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;

/**
 * The {@link PdxInstanceMethodInterceptor} class is a {@link MethodInterceptor} wrapping a {@link PdxInstance}
 * to back a proxy during intercepted method invocations.
 *
 * @author John Blum
 * @see org.aopalliance.intercept.MethodInterceptor
 * @see org.apache.geode.pdx.PdxInstance
 * @see org.apache.geode.pdx.WritablePdxInstance
 * @since 1.1.0
 */
public class PdxInstanceMethodInterceptor implements MethodInterceptor {

	private PdxInstance source;

	/**
	 * Factory method used to construct an instance of {@link PdxInstanceMethodInterceptor} initialized with
	 * the given {@link Object source}.
	 *
	 * @param source {@link Object} serving as the source to back the proxy in method invocations.
	 * Source must be an instance of {@link PdxInstance}.
	 * @return a new instance of {@link PdxInstanceMethodInterceptor} initialized with the given {@link Object source}.
	 * @throws IllegalArgumentException if {@link Object source} is not an instance of {@link PdxInstance}.
	 * @see #newPdxInstanceMethodInterceptor(PdxInstance)
	 */
	public static PdxInstanceMethodInterceptor newPdxInstanceMethodInterceptor(Object source) {
		Assert.isInstanceOf(PdxInstance.class, source, () -> String.format("Source [%1$s] is not an instance of [%2$s]",
			ObjectUtils.nullSafeClassName(source), PdxInstance.class.getName()));

		return newPdxInstanceMethodInterceptor((PdxInstance) source);
	}

	/**
	 * Factory method used to construct an instance of {@link PdxInstanceMethodInterceptor} initialized
	 * with the given {@link PdxInstance}.
	 *
	 * @param source {@link PdxInstance} serving as the source to back the proxy in method invocations.
	 * Source must not be {@literal null}.
	 * @return a new instance of {@link PdxInstanceMethodInterceptor} initialized with
	 * the given {@link PdxInstance source}.
	 * @throws IllegalArgumentException if {@link PdxInstance source} is {@literal null}.
	 * @see #PdxInstanceMethodInterceptor(PdxInstance)
	 * @see org.apache.geode.pdx.PdxInstance
	 */
	public static PdxInstanceMethodInterceptor newPdxInstanceMethodInterceptor(PdxInstance source) {
		return new PdxInstanceMethodInterceptor(source);
	}

	/**
	 * Constructs an instance of {@link PdxInstanceMethodInterceptor} initialized with
	 * the given {@link PdxInstance source}.
	 *
	 * @param source {@link PdxInstance} used as the source to back the proxy in method invocations.
	 * @throws IllegalArgumentException if {@link PdxInstance source} is {@literal null}.
	 * @see org.apache.geode.pdx.PdxInstance
	 */
	public PdxInstanceMethodInterceptor(PdxInstance source) {
		Assert.notNull(source, "Source must not be null");
		this.source = source;
	}

	/**
	 * Returns the {@link PdxInstance source} backing the proxy for intercepted method invocations.
	 *
	 * @return the {@link PdxInstance source} backing the proxy for intercepted method invocations.
	 * @see org.apache.geode.pdx.PdxInstance
	 */
	protected PdxInstance getSource() {
		return this.source;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Object invoke(MethodInvocation invocation) throws Throwable {
		Method method = invocation.getMethod();

		if (ReflectionUtils.isObjectMethod(method)) {
			return invocation.proceed();
		}
		else {
			Accessor methodAccessor = new Accessor(method);
			PdxInstance pdxInstance = getSource();
			String propertyName = methodAccessor.getPropertyName();

			Assert.state(pdxInstance.hasField(propertyName), () -> String.format(
				"Source [%1$s] does not contain field with name [%2$s]", pdxInstance, propertyName));

			if (methodAccessor.isGetter()) {
				return pdxInstance.getField(propertyName);
			}
			else { // is setter
				Assert.isTrue(invocation.getArguments().length == 1, () ->
					String.format("Invoked setter method [%1$s] must expect exactly 1 argument; Arguments were [%2$s]",
						method.getName(), Arrays.toString(invocation.getArguments())));

				Object value = invocation.getArguments()[0];

				WritablePdxInstance writablePdxInstance = pdxInstance.createWriter();

				Assert.state(writablePdxInstance != null, () -> String.format(
					"No writer for PdxInstance [%1$s] was found for setting field [%2$s] to value [%3$s]",
						pdxInstance, propertyName, value));

				writablePdxInstance.setField(propertyName, value);

				this.source = writablePdxInstance;

				return null;
			}
		}
	}
}
