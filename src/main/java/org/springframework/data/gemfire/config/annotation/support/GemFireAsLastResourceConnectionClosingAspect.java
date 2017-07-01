/*
 * Copyright 2017 the original author or authors.
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

import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.Aspect;

/**
 * The {@link GemFireAsLastResourceConnectionClosingAspect} class is a {@link AbstractGemFireAsLastResourceAspectSupport}
 * implementation responsible for closing the GemFire Connection obtained from the GemFire JCA ResourceAdapter
 * deployed in a managed environment when using GemFire as the Last Resource in a CMT/JTA Transaction
 * initiated from Spring's Transaction infrastructure.
 *
 * @author John Blum
 * @see org.aspectj.lang.annotation.Aspect
 * @see org.aspectj.lang.annotation.After
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractGemFireAsLastResourceAspectSupport
 * @since 2.0.0
 */
@SuppressWarnings("unused")
@Aspect
public class GemFireAsLastResourceConnectionClosingAspect extends AbstractGemFireAsLastResourceAspectSupport {

	private static final int DEFAULT_ORDER = 1024000;

	/**
	 * Closes the GemFire JCA ResourceAdapter Connection after the Spring CMT/JTA Transaction completes.
	 */
	@After("atTransactionalType() || atTransactionalMethod()")
	public void doGemFireConnectionClose() {

		logTraceInfo("Closing GemFire Connection...");

		GemFireConnectionHolder.close(isThrowOnError(), this::logWarning);
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
}
