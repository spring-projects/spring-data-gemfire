/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

import com.gemstone.gemfire.cache.query.QueryService;

/**
 * The CreateDefinedIndexesApplicationListener class is a Spring ApplicationListener used to create
 * all the GemFire Cache Region Indexes "defined" using the QueryService.defineXXXX(..) methods.
 *
 * @author John Blum
 * @see org.springframework.context.ApplicationListener
 * @see org.springframework.context.event.ContextRefreshedEvent
 * @see com.gemstone.gemfire.cache.query.QueryService
 * @since 1.7.0
 */
public class CreateDefinedIndexesApplicationListener implements ApplicationListener<ContextRefreshedEvent> {

	protected final Log logger = initLogger();

	/**
	 * Attempts to create all defined Indexes using the QueryService, defineXXXX(..) API once
	 * the Spring ApplicationContext has been refreshed.
	 *
	 * @param event the ContextRefreshedEvent fired when the Spring ApplicationContext gets refreshed.
	 * @see #getCache(org.springframework.context.event.ContextRefreshedEvent)
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 * @see com.gemstone.gemfire.cache.Cache#getQueryService()
	 * @see com.gemstone.gemfire.cache.client.ClientCache#getLocalQueryService()
	 * @see com.gemstone.gemfire.cache.query.QueryService#createDefinedIndexes()
	 */
	@Override
	public void onApplicationEvent(final ContextRefreshedEvent event) {
		try {
			QueryService localQueryService = getQueryService(event);

			if (localQueryService != null) {
				localQueryService.createDefinedIndexes();
			}
		}
		catch (Exception ignore) {
			logger.warn(String.format("unable to create defined Indexes (if any): %1$s", ignore.getMessage()));
		}
	}

	/* (non-Javadoc) */
	Log initLogger() {
		return LogFactory.getLog(getClass());
	}

	/* (non-Javadoc) */
	private QueryService getQueryService(final ContextRefreshedEvent event) {
		ApplicationContext localApplicationContext = event.getApplicationContext();

		return (localApplicationContext.containsBean(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE) ?
			localApplicationContext.getBean(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE,
				QueryService.class) : null);
	}

}
