/*
 * Copyright 2017-2019 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.annotation.support;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Optional;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.util.ObjectUtils;

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
	@Pointcut("target(org.apache.geode.cache.Region)")
	private void regionPointcut() {}

	/* (non-Javadoc) */
	@Pointcut("execution(* org.apache.geode.cache.Region.create(..))"
		+ " || execution(* org.apache.geode.cache.Region.get(..))"
		+ " || execution(* org.apache.geode.cache.Region.getAll(..))"
		+ " || execution(* org.apache.geode.cache.Region.getEntry(..))"
		+ " || execution(* org.apache.geode.cache.Region.invalidate(..))"
		+ " || execution(* org.apache.geode.cache.Region.keySet())"
		+ " || execution(* org.apache.geode.cache.Region.keySetOnServer())"
		+ " || execution(* org.apache.geode.cache.Region.localClear())"
		+ " || execution(* org.apache.geode.cache.Region.localDestroy(..))"
		+ " || execution(* org.apache.geode.cache.Region.localInvalidate(..))"
		+ " || execution(* org.apache.geode.cache.Region.put(..))"
		+ " || execution(* org.apache.geode.cache.Region.putAll(..))"
		+ " || execution(* org.apache.geode.cache.Region.putIfAbsent(..))"
		+ " || execution(* org.apache.geode.cache.Region.query(..))"
		+ " || execution(* org.apache.geode.cache.Region.remove(..))"
		+ " || execution(* org.apache.geode.cache.Region.removeAll(..))"
		+ " || execution(* org.apache.geode.cache.Region.replace(..))"
		+ " || execution(* org.apache.geode.cache.Region.selectValue(..))"
		+ " || execution(* org.apache.geode.cache.Region.size())"
		+ " || execution(* org.apache.geode.cache.Region.sizeOnServer())"
		+ " || execution(* org.apache.geode.cache.Region.values(..))"
		+ "")
	private void regionDataAccessPointcut() {}

	/* (non-Javadoc) */
	@Before("regionPointcut() && regionDataAccessPointcut()")
	public void regionDataAccessTracingAdvice(JoinPoint joinPoint) {
		getLogger().trace("Region data access call [{}(..)] with stack trace [{}]",
			toRegionMethodSignature(joinPoint), getCurrentThreadStackTrace());
	}

	/* (non-Javadoc) */
	private String toRegionMethodSignature(JoinPoint joinPoint) {

		return Optional.ofNullable(joinPoint)
			.map(JoinPoint::getSignature)
			.map(signature ->
				String.format("%1$s.%2$s", ObjectUtils.nullSafeClassName(joinPoint.getTarget()), signature.getName()))
			.orElse("");
	}
}
