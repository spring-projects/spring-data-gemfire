/*
 * Copyright 2002-2012 the original author or authors.
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
package org.springframework.data.gemfire.function.config;

import java.lang.reflect.Method;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.data.gemfire.function.PojoFunctionWrapper;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.execute.FunctionService;

/**
 * @author David Turanski
 *
 */
public abstract class GemfireFunctionUtils {
	private static Log log = LogFactory.getLog(GemfireFunctionUtils.class);
	
	public static void registerFunctionForPojoMethod(Object target, Method method, Map<String,Object> attributes, boolean overwrite) {
		String id = attributes.containsKey("id") ? (String)attributes.get("id") : "";
		
		PojoFunctionWrapper function = new PojoFunctionWrapper(target,method, id);
	    
		if (attributes.containsKey("HA")) {
	    	function.setHA((Boolean)attributes.get("HA"));
	    }
	    if (attributes.containsKey("optimizeForWrite")) {
	    	function.setOptimizeForWrite((Boolean)attributes.get("optimizeForWrite"));
	    }
	    
		if (FunctionService.isRegistered(function.getId())) {
			if (overwrite) {
				if (log.isDebugEnabled()) {
					log.debug("unregistering function definition " + function.getId());
				}
				FunctionService.unregisterFunction(function.getId());
			}
		}
		if (!FunctionService.isRegistered(function.getId())){
			FunctionService.registerFunction(function);
			if (log.isDebugEnabled()) {
				log.debug("registered function " + function.getId());
			}
		} else {
			if (log.isDebugEnabled()) {
				log.debug("function already registered " + function.getId());
			}
		}
	}
}
