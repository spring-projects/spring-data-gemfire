/*
 * Copyright 2017-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation.support;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newRuntimeException;

import javax.naming.NamingException;

import org.apache.geode.ra.GFConnectionFactory;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * The {@link GemFireAsLastResourceConnectionAcquiringAspect} class is a {@link AbstractGemFireAsLastResourceAspectSupport}
 * implementation responsible for acquiring a GemFire Connection from GemFire's JCA ResourceAdapter,
 * {@link GFConnectionFactory} after a CMT/JTA Transaction is began, which is initiated by
 * Spring's Transaction infrastructure.
 *
 * @author John Blum
 * @see org.apache.geode.ra.GFConnectionFactory
 * @see org.aspectj.lang.annotation.Aspect
 * @see org.aspectj.lang.annotation.Before
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractGemFireAsLastResourceAspectSupport
 * @since 2.0.0
 */
@SuppressWarnings("unused")
@Aspect
public class GemFireAsLastResourceConnectionAcquiringAspect extends AbstractGemFireAsLastResourceAspectSupport {

	private static final int DEFAULT_ORDER = 2048000;

	@Autowired(required = false)
	private GFConnectionFactory gemfireConnectionFactory;

	/**
	 * Acquires (opens) a GemFire JCA ResourceAdapter Connection after the Spring CMT/JTA Transaction begins.
	 */
	@Before("atTransactionalType() || atTransactionalMethod()")
	public void doGemFireConnectionFactoryGetConnection() {

		logTraceInfo("Acquiring GemFire Connection from GemFire JCA ResourceAdapter registered at [%s]...",
			resolveGemFireJcaResourceAdapterJndiName());

		GemFireConnectionHolder.acquire(resolveGemFireConnectionFactory(), isThrowOnError(), this::logError);
	}

	/* (non-Javadoc) */
	synchronized GFConnectionFactory resolveGemFireConnectionFactory() {

		GFConnectionFactory connectionFactory = getGemFireConnectionFactory();

		if (connectionFactory == null) {

			String resolvedGemFireJcaResourceAdapterJndiName = resolveGemFireJcaResourceAdapterJndiName();

			try {
				connectionFactory = this.gemfireConnectionFactory =
					(GFConnectionFactory) resolveContext().lookup(resolvedGemFireJcaResourceAdapterJndiName);
			}
			catch (NamingException cause) {
				throw newRuntimeException(cause,
					"Failed to resolve a GFConnectionFactory from the configured JNDI context name [%s]",
						resolvedGemFireJcaResourceAdapterJndiName);
			}
		}

		return connectionFactory;
	}


	/**
	 * Returns the default order used by this AOP Aspect in the chain of Aspects configured
	 * in Spring's Transaction Management.
	 *
	 * @return an int value specifying the default order used by this AOP Aspect in the chain of Aspects
	 * configured in Spring's Transaction Management.
	 */
	@Override
	protected Integer getDefaultOrder() {
		return DEFAULT_ORDER;
	}

	/**
	 * Returns a reference to the configured {@link GFConnectionFactory} instance.
	 *
	 * @return a reference to the configured {@link GFConnectionFactory} instance; may be {@literal null}.
	 * @see org.apache.geode.ra.GFConnectionFactory
	 */
	public synchronized GFConnectionFactory getGemFireConnectionFactory() {
		return this.gemfireConnectionFactory;
	}
}
