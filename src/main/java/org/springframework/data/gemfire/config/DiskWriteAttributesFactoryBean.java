/*
 * Copyright 2010-2011 the original author or authors.
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

import com.gemstone.gemfire.cache.DiskWriteAttributes;
import com.gemstone.gemfire.cache.DiskWriteAttributesFactory;

/**
 * Simple utility class used for defining nested factory-method like definitions
 * w/o polluting the container with useless beans.
 * 
 * @author Costin Leau
 * @deprecated
 */
@Deprecated
class DiskWriteAttributesFactoryBean implements FactoryBean<DiskWriteAttributes>, InitializingBean {

	private DiskWriteAttributes attributes;

	private DiskWriteAttributesFactory attrFactory;

	@Override
	public void afterPropertiesSet() {
		attributes = attrFactory.create();
	}

	@Override
	public DiskWriteAttributes getObject() throws Exception {
		return attributes;
	}

	@Override
	public Class<?> getObjectType() {
		return (attributes != null ? attributes.getClass() : DiskWriteAttributes.class);
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	public void setDiskAttributesFactory(DiskWriteAttributesFactory dwaf) {
		this.attrFactory = dwaf;
	}
}
