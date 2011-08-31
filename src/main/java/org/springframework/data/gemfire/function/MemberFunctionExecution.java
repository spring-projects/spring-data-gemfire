/*
 * Copyright 2002-2011 the original author or authors.
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
package org.springframework.data.gemfire.function;

import java.io.Serializable;

import com.gemstone.gemfire.cache.execute.Execution;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.FunctionService;
import com.gemstone.gemfire.distributed.DistributedSystem;

/**
 * @author David Turanski
 *
 */
public class MemberFunctionExecution extends FunctionExecution<Object> {
	

	private final DistributedSystem distributedSystem;

	/**
	 * @param functionId
	 * @param args
	 */
	public MemberFunctionExecution(DistributedSystem distributedSystem, Function function, Serializable... args) {
		super(function, args);
		this.distributedSystem = distributedSystem; 
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.FunctionExecution#getExecution()
	 */
	@Override
	protected Execution getExecution() {
		return FunctionService.onMember(this.distributedSystem, this.distributedSystem.getDistributedMember());
	}

 
	 

}
