/*
 * Copyright 2010-2020 the original author or authors.
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
package org.springframework.data.gemfire.function;

import java.util.List;

import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.FunctionService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.CollectionUtils;

/**
 * Spring FactoryBean for registering instance of Pivotal GemFire Function with the Pivotal GemFire FunctionService.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.apache.geode.cache.execute.Function
 * @see org.apache.geode.cache.execute.FunctionService
 */
public class FunctionServiceFactoryBean implements FactoryBean<FunctionService>, InitializingBean {

	private static Logger logger = LoggerFactory.getLogger(FunctionServiceFactoryBean.class);

	private List<Function> functions;

	@Override
	public void afterPropertiesSet() throws Exception {

		if (!CollectionUtils.isEmpty(this.functions)) {
			for (Function function : this.functions) {
				if (logger.isInfoEnabled()) {
					logger.info("registering Function with ID [{}]", function.getId());
				}
				FunctionService.registerFunction(function);
			}
		}
	}

	public void setFunctions(List<Function> functions) {
		this.functions = functions;
	}

	@Override
	public FunctionService getObject() throws Exception {
		return null;
	}

	@Override
	public Class<?> getObjectType() {
		return FunctionService.class;
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

}
