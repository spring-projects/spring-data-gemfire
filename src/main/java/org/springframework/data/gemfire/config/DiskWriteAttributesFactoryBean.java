/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.config;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;

/**
 * Simple utility class used for defining nested factory-method like definitions
 * w/o polluting the container with useless beans.
 * 
 * @author Costin Leau
 * @deprecated
 */
@Deprecated
@SuppressWarnings({ "deprecation", "unused" })
class DiskWriteAttributesFactoryBean implements FactoryBean<com.gemstone.gemfire.cache.DiskWriteAttributes>, InitializingBean {

	private com.gemstone.gemfire.cache.DiskWriteAttributes attributes;

	private com.gemstone.gemfire.cache.DiskWriteAttributesFactory attrFactory;

	@Override
	public void afterPropertiesSet() {
		attributes = attrFactory.create();
	}

	@Override
	public com.gemstone.gemfire.cache.DiskWriteAttributes getObject() throws Exception {
		return attributes;
	}

	@Override
	public Class<?> getObjectType() {
		return (attributes != null ? attributes.getClass() : com.gemstone.gemfire.cache.DiskWriteAttributes.class);
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	public void setDiskAttributesFactory(com.gemstone.gemfire.cache.DiskWriteAttributesFactory dwaf) {
		this.attrFactory = dwaf;
	}

}
