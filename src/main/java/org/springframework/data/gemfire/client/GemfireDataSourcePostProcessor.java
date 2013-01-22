/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.client;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.function.execution.GemfireFunctionOperations;
import org.springframework.data.gemfire.function.execution.GemfireOnServersFunctionTemplate;
import org.springframework.data.gemfire.repository.support.ListRegionsOnServerFunction;

import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;

/**
 * @author David Turanski
 *
 */
public class GemfireDataSourcePostProcessor implements BeanFactoryPostProcessor {
	private static Log logger = LogFactory.getLog(GemfireDataSourcePostProcessor.class);
	private final ClientCache cache;
	
	public GemfireDataSourcePostProcessor(ClientCache cache) {
		this.cache = cache;
	}
	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor#postProcessBeanFactory(org.springframework.beans.factory.config.ConfigurableListableBeanFactory)
	 */
	@Override
	public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
		createClientRegions(beanFactory);
		
	}
	
	private void createClientRegions(ConfigurableListableBeanFactory beanFactory) {
		GemfireFunctionOperations template = new GemfireOnServersFunctionTemplate(cache);
		Iterable<String> regionNames = template.executeAndExtract(new ListRegionsOnServerFunction());
		
		ClientRegionFactory<?,?> clientRegionFactory = null;
		if (regionNames !=null && regionNames.iterator().hasNext()) {
			clientRegionFactory = cache.createClientRegionFactory(ClientRegionShortcut.PROXY);
		}
		
		for (String regionName: regionNames) {
			if (logger.isDebugEnabled()) {
				logger.debug("creating client region for " + regionName);
				beanFactory.registerSingleton(regionName, 
				clientRegionFactory.create(regionName));
			}
		}
		
	}

}
