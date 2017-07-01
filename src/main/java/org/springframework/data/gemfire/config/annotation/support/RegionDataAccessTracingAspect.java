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

import java.io.PrintWriter;
import java.io.StringWriter;

import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The RegionDataAccessTracingAspect class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@Aspect
@SuppressWarnings("unused")
public class RegionDataAccessTracingAspect {

	private final Logger logger = newLogger();

	/* (non-Javadoc) */
	protected Logger newLogger() {
		return LoggerFactory.getLogger(getClass());
	}

	/* (non-Javadoc) */
	protected String getCurrentThreadStackTrace() {

		StringWriter writer = new StringWriter();

		new Throwable().printStackTrace(new PrintWriter(writer));

		return writer.toString();
	}

	/* (non-Javadoc) */
	protected Logger getLogger() {
		return this.logger;
	}

	/* (non-Javadoc) */
	@Pointcut("target(com.gemstone.gemfire.cache.Region)")
	private void regionPointcut() {}

	/* (non-Javadoc) */
	@Pointcut("execution(* com.gemstone.gemfire.cache.Region.create(..))"
		+ " || execution(* com.gemstone.gemfire.cache.Region.get(..))"
		+ " || execution(* com.gemstone.gemfire.cache.Region.getAll(..))"
		+ " || execution(* com.gemstone.gemfire.cache.Region.put(..))"
		+ " || execution(* com.gemstone.gemfire.cache.Region.putAll(..))"
		+ " || execution(* com.gemstone.gemfire.cache.Region.putIfAbsent(..))"
		+ " || execution(* com.gemstone.gemfire.cache.Region.remove(..))"
		+ " || execution(* com.gemstone.gemfire.cache.Region.replace(..))"
		+ " || execution(* com.gemstone.gemfire.cache.Region.selectValue(..))"
		+ " || execution(* com.gemstone.gemfire.cache.Region.values(..))"
		+ "")
	private void regionDataAccessPointcut() {}

	/* (non-Javadoc) */
	@Before("regionPointcut() && regionDataAccessPointcut()")
	public void regionDataAccessTracingAdvice() {
		getLogger().trace("Region data access call [{}]", getCurrentThreadStackTrace());
	}
}
