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

package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.concurrent.Executor;

import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.query.QueryService;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.util.ErrorHandler;

/**
 * The {@link EnableContinuousQueries} annotation marks a Spring {@link Configuration @Configuration} annotated
 * application configuration class to enable Pivotal GemFire / Apache Geode Continuous Queries (CQ) feature.
 *
 * @author John Blum
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.Target
 * @see java.util.concurrent.Executor
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.query.QueryService
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.ContinuousQueryConfiguration
 * @see org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer
 * @since @.0.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(ContinuousQueryConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableContinuousQueries {

	/**
	 * Refers to the {@link String name} of the declared {@link ErrorHandler} bean that will handle errors
	 * thrown during CQ event processing by CQ listeners.
	 *
	 * Defaults to unset.
	 */
	String errorHandlerBeanName() default "";

	/**
	 * Defines the Spring container lifecycle phase in which the SDG {@link ContinuousQueryListenerContainer}
	 * will be started on auto-start.
	 *
	 * Defaults to {@literal 0}.
	 */
	int phase() default 0;

	/**
	 * Refers to the name of the {@link Pool} over which CQs are registered and CQ events are received.
	 *
	 * Defaults to unset.
	 */
	String poolName() default "";

	/**
	 * Refers to the name of the {@link QueryService} bean used to define CQs.
	 *
	 * Defaults to unset.
	 */
	String queryServiceBeanName() default "";

	/**
	 * Refers to the name of the {@link Executor} bean used to process CQ events asynchronously.
	 *
	 * Defaults to unset.
	 */
	String taskExecutorBeanName() default "";

}
